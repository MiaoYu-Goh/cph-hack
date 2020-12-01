using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using Microsoft.CodeAnalysis.Operations;

namespace NativeArrayAnalyzer
{
    public static class SymbolExtensions
    {
        private static SymbolDisplayFormat QualifiedFormat { get; } =
            new SymbolDisplayFormat(
                typeQualificationStyle: SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces,
                genericsOptions: SymbolDisplayGenericsOptions.IncludeTypeParameters,
                miscellaneousOptions:
                SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers |
                SymbolDisplayMiscellaneousOptions.UseSpecialTypes);
        
        public static string ToFullName(this ITypeSymbol symbol)
        {
            return symbol.ToDisplayString(QualifiedFormat);
        }
    }
    
    public class NativeArrayRefVariableDeclaration
    {
        public string ContainingMethodName;
        public string RefVariableName;
        public string NativeArrayName;
        public bool IsReferencedNativeArrayDisposed;
    }
    
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class NativeArrayAnalyzer : DiagnosticAnalyzer
    {
        private readonly List<NativeArrayRefVariableDeclaration> _nativeArrayRefVariables = new List<NativeArrayRefVariableDeclaration>();
        
        public const string DiagnosticId = "NativeArrayAnalyzer";

        // You can change these strings in the Resources.resx file. If you do not want your analyzer to be localize-able, you can use regular strings for Title and MessageFormat.
        // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Localizing%20Analyzers.md for more on localization
        private static readonly LocalizableString Title = new LocalizableResourceString(nameof(Resources.AnalyzerTitle), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString MessageFormat = new LocalizableResourceString(nameof(Resources.AnalyzerMessageFormat), Resources.ResourceManager, typeof(Resources));
        private static readonly LocalizableString Description = new LocalizableResourceString(nameof(Resources.AnalyzerDescription), Resources.ResourceManager, typeof(Resources));
        private const string Category = "Naming";

        private static readonly DiagnosticDescriptor Warning =
            new DiagnosticDescriptor(DiagnosticId, Title, MessageFormat, Category, DiagnosticSeverity.Warning, isEnabledByDefault: true, description: Description);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics => ImmutableArray.Create(Warning);

        public override void Initialize(AnalysisContext context)
        {
            while (!Debugger.IsAttached)
            {
                Task.Delay(millisecondsDelay: 500).Wait();
            }

            context.RegisterSyntaxNodeAction(AnalyzeVariableDeclaration, SyntaxKind.VariableDeclaration);
            context.RegisterSyntaxNodeAction(AnalyzeExpressionStatement, SyntaxKind.ExpressionStatement);
            context.RegisterSyntaxNodeAction(AnalyzeIdentifierName, SyntaxKind.IdentifierName);
        }

        private void AnalyzeIdentifierName(SyntaxNodeAnalysisContext context)
        {
            var identifierNameSyntax = (IdentifierNameSyntax)context.Node;

            var containingMethod = identifierNameSyntax.Ancestors().OfType<MethodDeclarationSyntax>().First();

            var refVariableDeclaration = 
                _nativeArrayRefVariables.SingleOrDefault(
                    v => v.RefVariableName == identifierNameSyntax.Identifier.ValueText && v.ContainingMethodName == containingMethod.Identifier.ValueText);
            
            if (refVariableDeclaration != null && refVariableDeclaration.IsReferencedNativeArrayDisposed)
            {
                Diagnostic diagnostic = Diagnostic.Create(descriptor: Warning, location: identifierNameSyntax.GetLocation(), identifierNameSyntax.Identifier.ValueText);
                context.ReportDiagnostic(diagnostic);
            }
        }


        private void AnalyzeExpressionStatement(SyntaxNodeAnalysisContext context)
        {
            var expressionStatementSyntax = (ExpressionStatementSyntax)context.Node;
            var invocationExpressionSyntax = expressionStatementSyntax.ChildNodes().OfType<InvocationExpressionSyntax>().SingleOrDefault();

            // Dispose() does not take any arguments, so if we find any arguments, we know that we are not calling Dispose().
            if (invocationExpressionSyntax == null || invocationExpressionSyntax.ArgumentList.Arguments.Any()) 
            {
                return;
            }
            
            var memberAccessExpressionSyntax = invocationExpressionSyntax.ChildNodes().OfType<MemberAccessExpressionSyntax>().SingleOrDefault();

            if (memberAccessExpressionSyntax == null)
            {
                return;
            }

            var accessedObjectName = memberAccessExpressionSyntax.Expression as IdentifierNameSyntax;
            var containingMethod = expressionStatementSyntax.Ancestors().OfType<MethodDeclarationSyntax>().First();

            var refVariablesForNativeArray = 
                _nativeArrayRefVariables.Where(
                    v => 
                        v.ContainingMethodName == containingMethod.Identifier.ValueText
                        && v.NativeArrayName == accessedObjectName.Identifier.ValueText).ToArray();
            
            if (!refVariablesForNativeArray.Any())
            {
                return;
            }

            var invokedMethodName = memberAccessExpressionSyntax.Name as IdentifierNameSyntax;
            
            if (invokedMethodName.Identifier.ValueText == "Dispose")
            {
                foreach (var refVariable in refVariablesForNativeArray)
                {
                    refVariable.IsReferencedNativeArrayDisposed = true;
                }
            }
        }

        private void AnalyzeVariableDeclaration(SyntaxNodeAnalysisContext context)
        {
            var variableDeclarationSyntax = (VariableDeclarationSyntax)context.Node;
            if (!variableDeclarationSyntax.IsKind(SyntaxKind.RefKeyword))
            {
                return;
            }
            
            SemanticModel semanticModel = context.Compilation.GetSemanticModel(context.Node.SyntaxTree);

            var containingMethod = variableDeclarationSyntax.Ancestors().OfType<MethodDeclarationSyntax>().First();
            var variableDeclarator = variableDeclarationSyntax.ChildNodes().OfType<VariableDeclaratorSyntax>().Single();
            var refExpressionSyntax = variableDeclarator.ChildNodes().OfType<RefExpressionSyntax>().Single();
            var elementAccessExpressionSyntax = refExpressionSyntax.ChildNodes().OfType<ElementAccessExpressionSyntax>().Single();

            if (semanticModel.GetOperation(elementAccessExpressionSyntax) is IPropertyReferenceOperation propertyReferenceOperation)
            {
                var typeFullName = propertyReferenceOperation.Property.Type.ToFullName();
                
                if (!typeFullName.Contains("Unity.Collections.NativeArray"))
                {
                    return;
                }
                
                var identifierNameSyntax = elementAccessExpressionSyntax.Expression as IdentifierNameSyntax;
                _nativeArrayRefVariables.Add(
                    new NativeArrayRefVariableDeclaration
                    {
                        IsReferencedNativeArrayDisposed = false,
                        NativeArrayName = identifierNameSyntax.Identifier.ValueText,
                        RefVariableName = variableDeclarator.Identifier.ValueText,
                        ContainingMethodName = containingMethod.Identifier.ValueText
                    });
            }
        }
    }
}
