var _ = require('lodash');

export const ReturnType = {
  Step: "Step",

  Int: "int",
  String: "string",
  Boolean: "boolean",
  StringList: "List(String)",
  Unspecified: "Unspecified",
};

export const StepParameterType = {
  StepReturningExpression: 'StepReturningExpression',
  BooleanReturningExpression: 'BooleanReturningExpression',
  RegEx: 'RegEx',

  Boolean: 'boolean',
  Int: 'int',
};

export const StepFamily = {
  NodeTypeStep: "NodeTypeStep",
  ComplexStep: "ComplexStep",
  FilterStep: "FilterStep",
};

const makePropertyDirective = (queryComponent, returnType, description) => {
  return { queryComponent: queryComponent,  returnType: returnType, description: description };
};

export const PropertyDirectiveKind = {
  ArgumentIndex: "ArgumentIndex",
  Code: "Code",
  DynamicTypeHintFullName: "DynamicTypeHintFullName",
  EvaluationStrategy: "EvaluationStrategy",
  FullName: "FullName",
  ID: "ID",
  IsExternal: "IsExternal",
  Label: "Label",
  Language: "Language",
  LineNumber: "LineNumber",
  LineNumberEnd: "LineNumberEnd",
  Name: "Name",
  Order: "Order",
  Overlays: "Overlays",
  Signature: "Signature",
  Value: "Value",
  Version: "Version",
};


export const PropertyDirectives = {
  [PropertyDirectiveKind.ArgumentIndex]: makePropertyDirective('argIndex', ReturnType.Int, 'Identifies different AST children of CALL nodes or BLOCK nodes. Ordered 1 to N, with 0 reserved for implicit arguments like this or self'),
  [PropertyDirectiveKind.Code]: makePropertyDirective('code', ReturnType.String, 'The source code construct this node represents'),
  [PropertyDirectiveKind.DynamicTypeHintFullName]: makePropertyDirective('dynamicTypeHintFullName', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.EvaluationStrategy]: makePropertyDirective('evaluationStrategy', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.FullName]: makePropertyDirective('fullName', ReturnType.String, 'General string identifier which includes various details of the node it is defined on'),
  [PropertyDirectiveKind.ID]: makePropertyDirective('id', ReturnType.Int, 'Unique node identifier'),
  [PropertyDirectiveKind.IsExternal]: makePropertyDirective('isExternal', ReturnType.Boolean, 'Indicates that the node represents a program construct that is not defined directly in its source code'),
  [PropertyDirectiveKind.Label]: makePropertyDirective('label', ReturnType.String, 'Returns the value of the LABEL property which represents the node type'),
  [PropertyDirectiveKind.Language]: makePropertyDirective('language', ReturnType.String, 'The programming language this graph originates from'),
  [PropertyDirectiveKind.LineNumber]: makePropertyDirective('lineNumber', ReturnType.Int, 'First line at which the code representing this node is found'),
  [PropertyDirectiveKind.LineNumberEnd]: makePropertyDirective('lineNumberEnd', ReturnType.Int, 'Last line at which the code representing this node is found'),
  [PropertyDirectiveKind.Name]: makePropertyDirective('name', ReturnType.String, 'General string identifier for various nodes'),
  [PropertyDirectiveKind.Order]: makePropertyDirective('order', ReturnType.Int, 'General ordering property for AST nodes'),
  [PropertyDirectiveKind.Overlays]: makePropertyDirective('overlays', ReturnType.StringList, 'Names of Code Property Graph Overlays applied, in order of application'),
  [PropertyDirectiveKind.Signature]: makePropertyDirective('signature', ReturnType.String, 'The method signature; usually includes the method name, and the number, types and order of its parameters'),
  [PropertyDirectiveKind.Value]: makePropertyDirective('value', ReturnType.String, 'Generic string value container'),
  [PropertyDirectiveKind.Version]: makePropertyDirective('version', ReturnType.String, 'A version, given as a string'),
};

const makeStep = (queryComponent, family, description, sourceBasedOnly = false) => {
  return { queryComponent: queryComponent, family: family, description: description, sourceBasedOnly: sourceBasedOnly };
};


const makeParameterizedStep = (queryComponent, family, parameterType, returnType, description, sourceBasedOnly = false) => {
  return { queryComponent: queryComponent, family: family, parameterType: parameterType, returnType, description: description, sourceBasedOnly: sourceBasedOnly };
};

export const StepKind = {
  // node type steps
  All: "All",
  Block: "Block",
  Call: "Call",
  Comment: "Comment",
  File: "File",
  Identifier: "Identifier",
  Literal: "Literal",
  Local: "Local",
  Location: "Location",
  Member: "Member",
  MetaData: "MetaData",
  Method: "Method",
  MethodRef: "MethodRef",
  MethodReturn: "MethodReturn",
  Modifier: "Modifier",
  NamespaceBlock: "NamespaceBlock",
  Namespace: "Namespace",
  Parameter: "Parameter",
  Returns: "Returns",
  Tag: "Tag",
  TypeDecl: "TypeDecl",
  Typ: "Typ",

  // filter steps
  PropertyFilterStep: "PropertyFilterStep",
  WhereFilter: "WhereFilter",
  WhereNonEmptyFilter: "WhereNonEmptyFilter",
  FilterFilter: "FilterFilter",
  FilterNotFilter: "FilterNotFilter",

  // complex steps
  Argument: "Argument" ,
  AliasTypeDecl: "AliasTypeDecl",
  AliasTypeFullName: "AliasTypeFullName",
  AliasType: "AliasType",
  AliasTypeDeclTransitive: "AliasTypeDeclTransitive",
  AliasTypeTransitive: "AliasTypeTransitive",
  BaseTypeDecl: "BaseTypeDecl",
  BaseTypeDeclTransitive: "BaseTypeDeclTransitive",
  BaseType: "BaseType",
  BindingTypeDecl: "BindingTypeDecl",
  DerivedTypeDecl: "DerivedTypeDecl",
  DerivedType: "DerivedType",
  DerivedTypeDeclTransitive: "DerivedTypeDeclTransitive",
  DerivedTypeTransitive: "DerivedTypeTransitive",
  IsCfgNode: "IsCfgNode",
  IsCallTo: "IsCallTo",
  IsDynamic: "IsDynamic",
  IsDefined: "IsDefined",
  IsEmpty: "IsEmpty",
  IsPrivate: "IsPrivate",
  IsProtected: "IsProtected",
  IsPublic: "IsPublic",
  IsReturn: "IsReturn",
  IsStatic: "IsStatic",
  IsVirtual: "IsVirtual",
  MethodBinding: "MethodBinding" ,
  ReferencedTypeDecl: "ReferencedTypeDecl"
};

export const Steps = {
  // node type steps
  [StepKind.All]: makeStep('all', StepFamily.NodeTypeStep, 'Visits all nodes in the Code Property Graph'),
  [StepKind.Block]: makeStep('block', StepFamily.NodeTypeStep, 'Visits BLOCK nodes'),
  [StepKind.Call]: makeStep('call', StepFamily.NodeTypeStep, 'Visits CALL nodes; represent call-sites'),
  [StepKind.Comment]: makeStep('comment', StepFamily.NodeTypeStep, 'Visits COMMENT nodes; COMMENT nodes exist only for source-based Code Propert Graphs', true),
  [StepKind.File]: makeStep('file', StepFamily.NodeTypeStep, 'Visits FILE nodes; in source-based Code Property Graphs, FILE nodes will point both to the actual source code files of the program under analysis and paths to the referenced files from the standard library, for IR-based Code Property Graphs, the nodes representing source code files will not exist'),
  [StepKind.Identifier]: makeStep('identifier', StepFamily.NodeTypeStep, 'Visits IDENTIFIER nodes; e.g. occurrences of local variables or class members in method bodies '),
  [StepKind.Literal]: makeStep('literal', StepFamily.NodeTypeStep, 'Visits LITERAL nodes; e.g. numbers or strings'),
  [StepKind.Local]: makeStep('local', StepFamily.NodeTypeStep, 'Visits LOCAL nodes; represent local variable'),
  [StepKind.Member]: makeStep('member', StepFamily.NodeTypeStep, 'Visits MEMBER nodes; MEMBER nodes refer to members of complex types like classes or structs'),
  [StepKind.MetaData]: makeStep('metaData', StepFamily.NodeTypeStep, 'Visits the META_DATA node'),
  [StepKind.Method]: makeStep('method', StepFamily.NodeTypeStep, 'Visits METHOD nodes'),
  [StepKind.MethodRef]: makeStep('methodRef', StepFamily.NodeTypeStep, 'Visits METHOD_REF nodes'),
  [StepKind.MethodReturn]: makeStep('methodReturn', StepFamily.NodeTypeStep, 'Visits METHOD_RETURN nodes; all formal return parameters'),
  [StepKind.Modifier]: makeStep('modifier', StepFamily.NodeTypeStep, 'Visits MODIFIER nodes; e.g. public, private, static'),
  [StepKind.Namespace]: makeStep('namespace', StepFamily.NodeTypeStep, 'Visits NAMESPACE nodes'),
  [StepKind.NamespaceBlock]: makeStep('namespaceBlock', StepFamily.NodeTypeStep, 'Visits NAMESPACE_BLOCK nodes'),
  [StepKind.Parameter]: makeStep('parameter', StepFamily.NodeTypeStep, 'Visits PARAMETER nodes'),
  [StepKind.Returns]: makeStep('returns', StepFamily.NodeTypeStep, 'Visits RETURN nodes'),
  [StepKind.TypeDecl]: makeStep('typeDecl', StepFamily.NodeTypeStep, 'Visits TYPE_DECL nodes'),
  [StepKind.Tag]: makeStep('tag', StepFamily.NodeTypeStep, 'Visits TAG nodes'),
  [StepKind.Typ]: makeStep('typ', StepFamily.NodeTypeStep, 'Visits TYPE nodes'),

  // filter step
  [StepKind.FilterFilter]: makeParameterizedStep('filter', StepFamily.FilterStep, StepParameterType.StepReturningExpression, ReturnType.Step, 'TODO'),
  [StepKind.FilterNotFilter]: makeParameterizedStep('filterNot', StepFamily.FilterStep, StepParameterType.StepReturningExpression, ReturnType.Step, 'TODO'),
  [StepKind.WhereFilter]: makeParameterizedStep('where', StepFamily.FilterStep, StepParameterType.BooleanReturningExpression, ReturnType.Step, 'TODO'),
  [StepKind.WhereNonEmptyFilter]: makeParameterizedStep('whereNonEmpty', StepFamily.FilterStep, StepParameterType.StepReturningExpression, ReturnType.Step, 'TODO'),

  // complex steps
  [StepKind.AliasTypeDecl]: makeStep('aliasTypeDecl', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasTypeFullName]: makeStep('aliasTypeFullName', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasType]: makeStep('aliasType', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasTypeDeclTransitive]: makeStep('aliasTypeDeclTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasTypeTransitive]: makeStep('aliasTypeTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.Argument]: makeStep('argument', StepFamily.ComplexStep, 'Visits nodes connected by ARGUMENT edges; actual parameters'),
  [StepKind.BaseTypeDecl]: makeStep('baseTypeDecl', StepFamily.ComplexStep, 'TODO'),
  [StepKind.BaseTypeDeclTransitive]: makeStep('baseTypeDeclTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.BaseTypeTransitive]: makeStep('baseTypeTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.BaseType]: makeStep('baseType', StepFamily.ComplexStep, 'TODO'),
  [StepKind.BindingTypeDecl]: makeStep('bindingTypeDecl', StepFamily.ComplexStep, 'TODO'),
  [StepKind.DerivedTypeDecl]: makeStep('derivedTypeDecl', StepFamily.ComplexStep, 'TODO'),
  [StepKind.DerivedType]: makeStep('derivedType', StepFamily.ComplexStep, 'TODO'),
  [StepKind.DerivedTypeDeclTransitive]: makeStep('derivedTypeDeclTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.DerivedTypeTransitive]: makeStep('derivedTypeTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsCfgNode]: makeStep('isCfgNode', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsCallTo]: makeStep('isCallTo', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsDefined]: makeStep('isDefined', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsDynamic]: makeStep('isDynamic', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsEmpty]: makeStep('isEmpty', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsPrivate]: makeStep('isPrivate', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsProtected]: makeStep('isProtected', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsPublic]: makeStep('isPublic', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsReturn]: makeStep('isReturn', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsStatic]: makeStep('isStatic', StepFamily.ComplexStep, 'TODO'),
  [StepKind.IsVirtual]: makeStep('isVirtual', StepFamily.ComplexStep, 'TODO'),
  [StepKind.Location]: makeStep('location', StepFamily.ComplexStep, 'TODO'),
  [StepKind.MethodBinding]: makeStep('methodBinding', StepFamily.ComplexStep, 'TODO'),
  [StepKind.ReferencedTypeDecl]: makeStep('referencedTypeDecl', StepFamily.ComplexStep, 'TODO'),
};

export const PropertyDirectivesConnections = {
  [StepKind.All]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
  ],
  [StepKind.Block]: [
    { kind: PropertyDirectiveKind.ArgumentIndex, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Call]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
  ],
  [StepKind.Comment]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
  ],
  [StepKind.File]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Identifier]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Literal]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Local]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Member]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.MetaData]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Language, contextualDescription: null },
    { kind: PropertyDirectiveKind.Version, contextualDescription: null },
  ],
  [StepKind.Method]: [
    { kind: PropertyDirectiveKind.FullName, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.IsExternal, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumberEnd, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
    { kind: PropertyDirectiveKind.Signature, contextualDescription: null },
  ],
  [StepKind.MethodRef]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.MethodReturn]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Modifier]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Namespace]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.NamespaceBlock]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.FullName, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Parameter]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Returns]: [
    { kind: PropertyDirectiveKind.Code, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
  [StepKind.Tag]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Value, contextualDescription: null },
  ],
  [StepKind.Typ]: [
    { kind: PropertyDirectiveKind.FullName, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
  ],
  [StepKind.TypeDecl]: [
    { kind: PropertyDirectiveKind.FullName, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.IsExternal, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, contextualDescription: null },
  ],
};


export const StepConnections = {
  [StepKind.All]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: 'Visits FILE nodes attached to all nodes in the graph' },
    { kind: StepKind.Tag, contextualDescription: 'Visits TAG nodes attached to all nodes in the graph' },
  ],
  [StepKind.Argument]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Block]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Local, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
],
  [StepKind.Call]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.Argument, contextualDescription: null },
    { kind: StepKind.IsDynamic, contextualDescription: null },
    { kind: StepKind.IsStatic, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Comment]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.File]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Namespace, contextualDescription: null },
    { kind: StepKind.NamespaceBlock, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Identifier]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Literal]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
  ],
  [StepKind.Local]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Member]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Modifier, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.MetaData]: [
    // node type steps
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Method]: [
    // node type steps
    { kind: StepKind.Local, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.MethodReturn, contextualDescription: null },
    { kind: StepKind.Modifier, contextualDescription: null },
    { kind: StepKind.Namespace, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: 'Visit TAG nodes attached to the METHOD nodes; that is, tags found on methods' },

    // complex steps
    { kind: StepKind.IsPrivate, contextualDescription: 'Filter for the METHOD nodes which are connected to MODIFIER nodes with the modifierType property set to PRIVATE' },
    { kind: StepKind.IsProtected, contextualDescription: 'Filter for the METHOD nodes which are connected to MODIFIER nodes with the modifierType property set to PROTECTED' },
    { kind: StepKind.IsPublic, contextualDescription: 'Filter for the METHOD nodes which are connected to MODIFIER nodes with the modifierType property set to PUBLIC' },
    { kind: StepKind.IsStatic, contextualDescription: 'Filter for the METHOD nodes which are connected to MODIFIER nodes with the modifierType property set to STATIC' },
    { kind: StepKind.IsVirtual, contextualDescription: 'Filter for the METHOD nodes which are connected to MODIFIER nodes with the modifierType property set to VIRTUAL' },
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.MethodRef]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.MethodReturn]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Modifier]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Namespace]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.NamespaceBlock]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Parameter]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Returns]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.TypeDecl]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Member, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.MethodBinding, contextualDescription: null },
    { kind: StepKind.Modifier, contextualDescription: null },
    { kind: StepKind.Namespace, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.AliasTypeDecl, contextualDescription: null },
    { kind: StepKind.AliasTypeDeclTransitive, contextualDescription: null },
    { kind: StepKind.BaseType, contextualDescription: null },
    { kind: StepKind.BaseTypeDecl, contextualDescription: null },
    { kind: StepKind.BaseTypeDeclTransitive, contextualDescription: null },
    { kind: StepKind.DerivedTypeDecl, contextualDescription: null },
    { kind: StepKind.DerivedTypeDeclTransitive, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Tag]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: 'Visits CALL nodes attached to the TAG nodes' },
    { kind: StepKind.File, contextualDescription: 'Visits FILE nodes attached to the TAG nodes' },
    { kind: StepKind.Literal, contextualDescription: 'Visits LITERAL nodes attached to the TAG nodes' },
    { kind: StepKind.Local, contextualDescription: 'Visits LOCAL nodes attached to the TAG nodes' },
    { kind: StepKind.Method, contextualDescription: 'Visits METHOD nodes attached to the TAG nodes' },
    { kind: StepKind.MethodReturn, contextualDescription: 'Visits METHOD_RETURN nodes attached to the TAG nodes' },
    { kind: StepKind.Parameter, contextualDescription: 'Visits PARAMETER nodes attached to the TAG nodes' },
    { kind: StepKind.Tag, contextualDescription: 'Visits TAG nodes attached to the TAG nodes' },

    // complex steps
    { kind: StepKind.Location, contextualDescription: null },
  ],
  [StepKind.Typ]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Member, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.AliasType, contextualDescription: null },
    { kind: StepKind.AliasTypeDecl, contextualDescription: null },
    { kind: StepKind.AliasTypeTransitive, contextualDescription: null },
    { kind: StepKind.BaseType, contextualDescription: null },
    { kind: StepKind.BaseTypeTransitive, contextualDescription: null },
    { kind: StepKind.DerivedType, contextualDescription: null },
    { kind: StepKind.DerivedTypeDecl, contextualDescription: null },
    { kind: StepKind.DerivedTypeTransitive, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.ReferencedTypeDecl, contextualDescription: null },
  ],
};


const OperatorKind = {
  Addition: "<operator>.addition",
  AddressOf: "<operator>.addressOf",
  And: "<operator>.and",
  ArithmeticShiftRight: "<operator>.arithmeticShiftRight",
  AssignmentAnd: "<operators>.assignmentAnd",
  AssignmentArithmeticShiftRight: "<operators>.assignmentArithmeticShiftRight",
  AssignmentDivision: "<operator>.assignmentDivision",
  AssignmentExponentiation: "<operators>.assignmentExponentiation",
  AssignmentLogicalShiftRight: "<operators>.assignmentLogicalShiftRight",
  AssignmentMinus: "<operator>.assignmentMinus",
  AssignmentModulo: "<operators>.assignmentModulo",
  AssignmentMultiplication: "<operator>.assignmentMultiplication",
  Assignment: "<operator>.assignment",
  AssignmentOr: "<operators>.assignmentOr",
  AssignmentPlus: "<operator>.assignmentPlus",
  AssignmentShiftLeft: "<operators>.assignmentShiftLeft",
  AssignmentXor: "<operators>.assignmentXor",
  Cast: "<operator>.cast",
  Compare: "<operator>.compare",
  ComputedMemberAccess: "<operator>.computedMemberAccess",
  Conditional: "<operator>.conditional",
  Delete: "<operator>.delete",
  Division: "<operator>.division",
  Equals: "<operator>.equals",
  Exponentiation: "<operator>.exponentiation",
  FieldAccess: "<operator>.fieldAccess",
  GetElementPtr: "<operator>.getElementPtr",
  GreaterEqualsThan: "<operator>.greaterEqualsThan",
  GreaterThan: "<operator>.greaterThan",
  IndexAccess: "<operator>.indexAccess",
  IndirectComputedMemberAccess: "<operator>.indirectComputedMemberAccess",
  IndirectFieldAccess: "<operator>.indirectFieldAccess",
  IndirectIndexAccess: "<operator>.indirectIndexAccess",
  Indirection: "<operator>.indirection",
  IndirectMemberAccess: "<operator>.indirectMemberAccess",
  InstanceOf: "<operator>.instanceOf",
  LessEqualsThan: "<operator>.lessEqualsThan",
  LessThan: "<operator>.lessThan",
  LogicalAnd: "<operator>.logicalAnd",
  LogicalNot: "<operator>.logicalNot",
  LogicalOr: "<operator>.logicalOr",
  LogicalShiftRight: "<operator>.logicalShiftRight",
  MemberAccess: "<operator>.memberAccess",
  Minus: "<operator>.minus",
  Modulo: "<operator>.modulo",
  Multiplication: "<operator>.multiplication",
  NotEquals: "<operator>.notEquals",
  Not: "<operator>.not",
  Or: "<operator>.or",
  Plus: "<operator>.plus",
  PointerShift: "<operator>.pointerShift",
  PostDecrement: "<operator>.postDecrement",
  PostIncrement: "<operator>.postIncrement",
  PreDecrement: "<operator>.preDecrement",
  PreIncrement: "<operator>.preIncrement",
  ShiftLeft: "<operator>.shiftLeft",
  SizeOf: "<operator>.sizeOf",
  Subtraction: "<operator>.subtraction",
  Xor: "<operator>.xor",
};


export const PropertyFilterStepConnections = {
  [StepKind.Block]: [
    { kind: PropertyDirectiveKind.ArgumentIndex, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Call]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
  ],
  [StepKind.Comment]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.File]: [
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Identifier]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Literal]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Local]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Member]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Method]: [
    { kind: PropertyDirectiveKind.FullName, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.IsExternal, parameterType: StepParameterType.Boolean, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumberEnd, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Signature, parameterType: StepParameterType.RegEx, contextualDescription: null },
  ],
  [StepKind.MethodRef]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.MethodReturn]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Modifier]: [
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Namespace]: [
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.NamespaceBlock]: [
    { kind: PropertyDirectiveKind.FullName, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Parameter]: [
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Returns]: [
    { kind: PropertyDirectiveKind.Code, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, parameterType: StepParameterType.Int, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
  [StepKind.Tag]: [
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Value, parameterType: StepParameterType.RegEx, contextualDescription: null },
  ],
  [StepKind.Typ]: [
    { kind: PropertyDirectiveKind.FullName, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
  ],
  [StepKind.TypeDecl]: [
    { kind: PropertyDirectiveKind.FullName, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.IsExternal, parameterType: StepParameterType.Boolean, contextualDescription: null },
    { kind: PropertyDirectiveKind.Name, parameterType: StepParameterType.RegEx, contextualDescription: null },
    { kind: PropertyDirectiveKind.Order, parameterType: StepParameterType.Int, contextualDescription: null },
  ],
};


export const FilterStepConnections = {
  [StepKind.All]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Block]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Call]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Comment]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.File]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Identifier]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Literal]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Local]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Member]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Method]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.MethodRef]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.MethodReturn]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Modifier]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Namespace]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.NamespaceBlock]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Parameter]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Returns]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Tag]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.Typ]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
  [StepKind.TypeDecl]: [
    { kind: StepKind.FilterFilter, contextualDescription: null },
    { kind: StepKind.FilterNotFilter, contextualDescription: null },
    { kind: StepKind.WhereFilter, contextualDescription: null },
    { kind: StepKind.WhereNonEmptyFilter, contextualDescription: null },
  ],
};



class CPGQLRef {
  constructor(steps, propertyDirectives, connections) {
    this.steps = steps;

    const stepsInfo = {};

    const stepForKind = (kind) => {
      const step = _.cloneDeep(steps[kind]);
      step.kind = kind;
      return step;
    };

    const propertyDirectiveForKind = (kind) => {
      const propertyDirective = _.cloneDeep(propertyDirectives[kind]);
      propertyDirective.kind = kind;
      return propertyDirective;
    };

    for (const [ stepKind, step ] of Object.entries(steps)) {
      const singleStepInfo = {
        kind: stepKind,
        family: step.family,
        queryComponent: step.queryComponent,
        description: step.description,
        propertyDirectives: [],
        allowedSteps: []
      };

      const allowedSteps = [];
      const propertyDirectivesForStep = [];
      const filterSteps = [];

      const propertyDirectiveConnections = connections.stepsToPropertyDirectives[stepKind];
      if ( propertyDirectiveConnections != null ) {
        propertyDirectiveConnections.forEach((connection) => {
          const propertyDirective = propertyDirectiveForKind(connection.kind);
          const contextualDescription = connection.contextualDescription;
          if ( contextualDescription != null ) {
            propertyDirectivesForStep.description = contextualDescription;
          };
          propertyDirectivesForStep.push(propertyDirective);
        });
      };

      const stepConnections = connections.stepsToNodeTypeSteps[stepKind];
      if ( stepConnections != null ) {
        stepConnections.forEach((connection) => {
          const stepForConnection = stepForKind(connection.kind);
          const stepDescription = stepForConnection.description;
          const contextualDescription = connection.contextualDescription;
          if ( contextualDescription != null ) {
            stepForConnection.description = contextualDescription;
          }
          allowedSteps.push(stepForConnection);
        });
      };

      singleStepInfo.propertyDirectives = propertyDirectivesForStep;

      const propertyFilterStepsConnections = connections.stepsToPropertyFilterSteps[stepKind];
      if ( propertyFilterStepsConnections != null ) {
        propertyFilterStepsConnections.forEach((connection) => {
          const propertyDirective = propertyDirectives[connection.kind];
          if ( propertyDirective != null ) {
            const filterStep = {
              family: StepFamily.FilterStep,
              parameterType: connection.parameterType,
              queryComponent: propertyDirective.queryComponent,
              returnType: propertyDirective.returnType,
              description: connection.contextualDescription,
              kind: StepKind.PropertyFilterStep,
            };
            filterSteps.push(filterStep);
          }
        });
      }

      const filterStepsConnections = connections.stepsToFilterSteps[stepKind];
      if ( filterStepsConnections != null ) {
        filterStepsConnections.forEach((connection) => {
          const stepForConnection = stepForKind(connection.kind);
          if ( stepForConnection != null ) {
            const filterStep = {
              family: StepFamily.FilterStep,
              parameterType: stepForConnection.parameterType,
              returnType: stepForConnection.returnType,
              queryComponent: stepForConnection.queryComponent,
              description: connection.contextualDescription,
              kind: stepForConnection.kind,
            };
            filterSteps.push(filterStep);
          }
        });
      }

      singleStepInfo.allowedSteps = allowedSteps;

      singleStepInfo.filterSteps = filterSteps;

      singleStepInfo.nodeTypeSteps = allowedSteps.filter((step) => {
        return step.family == StepFamily.NodeTypeStep;
      });

      singleStepInfo.complexSteps = allowedSteps.filter((step) => {
        return step.family == StepFamily.ComplexStep;
      });

      stepsInfo[stepKind] = singleStepInfo;
    }

    this.stepsInfo = stepsInfo;
  }

  stepInfoForKind(kind) {
    if (!Object.keys(StepKind).includes(kind)) {
      return null;
    }
    return this.stepsInfo[kind];
  }

  stepsInfoForFamily(family) {
    if (!Object.keys(StepFamily).includes(family)) {
      return null;
    }

    let stepsInfo = [];
    for (const [ stepKind, stepInfo ] of Object.entries(this.stepsInfo)) {
      if (stepInfo.family == family) {
        stepsInfo.push(stepInfo);
      }
    }
    return stepsInfo;
  }
}

export const Connections = {
  stepsToPropertyDirectives: PropertyDirectivesConnections,
  stepsToNodeTypeSteps: StepConnections,
  stepsToPropertyFilterSteps: PropertyFilterStepConnections,
  stepsToFilterSteps: FilterStepConnections,
};

export const cpgqlRef = new CPGQLRef(
  Steps,
  PropertyDirectives,
  Connections,
);

export default cpgqlRef;


/**
 * The following table lists all node classes generated
 * in `codepropertygraph`, together with a 1/0 boolean value
 * that denotes whether or not the node has the highest priority
 * in regards to the documentation.
 *
 * Dataflow nodes are left out for this first iteration
 *

|----------------------------------------
| Node Class                 | Important
|----------------------------------------
| AnnotationLiteral          |   0
| AnnotationParameterAssign  |   0
| AnnotationParameter        |   0
| Annotation                 |   0
| ArrayInitializer           |   0
| Binding                    |   1
| Block                      |   1
| CallChain                  |   1
| Call                       |   1
| CallSite                   |   1
| ClosureBinding             |   0
| Comment                    |   1
| ConfigFile                 |   1
| ControlStructure           |   1
| Dependency                 |   1
| DetachedTrackingPoint      |   0
| DomAttribute               |   0
| DomNode                    |   0
| FieldIdentifier            |   0
| File                       |   1
| Finding                    |   1
| Flow                       |   0
| FrameworkData              |   0
| Framework                  |   1
| Identifier                 |   1
| ImplicitCall               |   1
| Ioflow                     |   0
| JumpTarget                 |   0
| KeyValuePair               |   0
| Literal                    |   1
| Local                      |   1
| Location                   |   1
| MatchInfo                  |   0
| Member                     |   1
| MetaData                   |   1
| MethodInst                 |   1
| MethodParameterIn          |   1
| MethodParameterOut         |   1
| MethodRef                  |   1
| MethodReturn               |   1
| Method                     |   1
| MethodSummary              |   0
| Modifier                   |   1
| NamespaceBlock             |   1
| Namespace                  |   1
| NewNodes                   |   0
| PackagePrefix              |   0
| package                    |   1
| ProgramPoint               |   0
| Read                       |   0
| Return                     |   1
| Route                      |   0
| SensitiveDataType          |   0
| SensitiveMember            |   0
| SensitiveReference         |   0
| SensitiveVariable          |   0
| Sink                       |   0
| Source                     |   0
| SpAnnotationParameter      |   0
| SpBlacklist                |   0
| TagNodePair                |   0
| Tag                        |   1
| Tags                       |   0
| Transformation             |   0
| Transform                  |   0
| TypeArgument               |   1
| TypeDecl                   |   1
| TypeParameter              |   1
| Type                       |   1
| Unknown                    |   1
| VariableInfo               |   0
| Write                      |   0
--------------------------------------
**/
