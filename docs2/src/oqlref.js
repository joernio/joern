export const ReturnType = {
  Int: "int",
  String: "string",
  Boolean: "boolean",
  StringList: "List(String)",
  Unspecified: "Unspecified",
};

export const StepFamily = {
  NodeTypeStep: "NodeTypeStep",
  ComplexStep: "ComplexStep",
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
  InheritsFromTypeFullName: "InheritsFromTypeFullName",
  Label: "Label",
  Language: "Language",
  LineNumber: "LineNumber",
  Name: "Name",
  Order: "Order",
  Overlays: "Overlays",
  Signature: "Signature",
  SPID: "SPID",
  TypeDeclFullName: "TypeDeclFullName",
  TypeFullName: "TypeFullName",
  Value: "Value",
  Version: "Version",
};


export const PropertyDirectives = {
  [PropertyDirectiveKind.ArgumentIndex]: makePropertyDirective('argumentIndex', ReturnType.Int, 'TODO'),
  [PropertyDirectiveKind.Code]: makePropertyDirective('code', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.DynamicTypeHintFullName]: makePropertyDirective('dynamicTypeHintFullName', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.EvaluationStrategy]: makePropertyDirective('evaluationStrategy', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.FullName]: makePropertyDirective('fullName', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.ID]: makePropertyDirective('id', ReturnType.Int, 'Returns an integer that uniquely identifies the node in its Code Property Graph'),
  [PropertyDirectiveKind.IsExternal]: makePropertyDirective('isExternal', ReturnType.Boolean, 'TODO'),
  [PropertyDirectiveKind.InheritsFromTypeFullName]: makePropertyDirective('inheritsFromTypeFullname', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.Label]: makePropertyDirective('label', ReturnType.String, 'Returns the value of the LABEL property with represents the node type'),
  [PropertyDirectiveKind.Language]: makePropertyDirective('language', ReturnType.String, 'The programming language this graph originates from'),
  [PropertyDirectiveKind.LineNumber]: makePropertyDirective('lineNumber', ReturnType.Int, 'TODO'),
  [PropertyDirectiveKind.Name]: makePropertyDirective('name', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.Order]: makePropertyDirective('order', ReturnType.Int, 'TODO'),
  [PropertyDirectiveKind.Overlays]: makePropertyDirective('overlays', ReturnType.StringList, 'Names of overlays applied to this Code Property Graph, in order of application'),
  [PropertyDirectiveKind.Signature]: makePropertyDirective('signature', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.SPID]: makePropertyDirective('spId', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.TypeDeclFullName]: makePropertyDirective('typeDeclFullName', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.TypeFullName]: makePropertyDirective('typeFullName', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.Value]: makePropertyDirective('value', ReturnType.String, 'TODO'),
  [PropertyDirectiveKind.Version]: makePropertyDirective('version', ReturnType.String, 'TODO'),
};

const makeStep = (queryComponent, family, description, sourceBasedOnly = false) => {
  return { queryComponent: queryComponent, family: family, description: description, sourceBasedOnly: sourceBasedOnly };
};

export const StepKind = {
  // node type steps
  All: "All" ,
  Argument: "Argument" ,
  Block: "Block" ,
  Call: "Call" ,
  Comment: "Comment" ,
  File: "File" ,
  Identifier: "Identifier" ,
  Literal: "Literal" ,
  Local: "Local" ,
  Location: "Location" ,
  Member: "Member" ,
  MetaData: "MetaData" ,
  Method: "Method" ,
  MethodBinding: "MethodBinding" ,
  MethodRef: "MethodRef" ,
  MethodReturn: "MethodReturn" ,
  Modifier: "Modifier" ,
  Namespace: "Namespace" ,
  NamespaceBlock: "NamespaceBlock" ,
  Parameter: "Parameter" ,
  Returns: "Returns" ,
  TypeDecl: "TypeDecl" ,
  Tag: "Tag" ,
  Typ: "Typ" ,
  Types: "Types" ,

  // complex steps
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
  ReferencedTypeDecl: "ReferencedTypeDecl"
};

export const Steps = {
  [StepKind.All]: makeStep('all', StepFamily.NodeTypeStep, 'All nodes'),
  [StepKind.Argument]: makeStep('argument', StepFamily.NodeTypeStep, 'All arguments (actual parameters)'),
  [StepKind.Block]: makeStep('block', StepFamily.NodeTypeStep, 'TODO'),
  [StepKind.Call]: makeStep('call', StepFamily.NodeTypeStep, 'All call sites'),
  [StepKind.Comment]: makeStep('comment', StepFamily.NodeTypeStep, 'All comments (only source-based)', true),
  [StepKind.File]: makeStep('file', StepFamily.NodeTypeStep, 'All source files. For source-based Code Property Graphs, returns the files of the program under analysis, for the intermediate-representation Code Property Graphs, it only returns the files of the standard library that it references.'),
  [StepKind.Identifier]: makeStep('identifier', StepFamily.NodeTypeStep, 'All identifiers, e.g.  occurrences of local variables or class members in method bodies '),
  [StepKind.Literal]: makeStep('literal', StepFamily.NodeTypeStep, 'All literals, e.g. numbers or strings'),
  [StepKind.Local]: makeStep('local', StepFamily.NodeTypeStep, 'All local variables '),
  [StepKind.Location]: makeStep('location', StepFamily.NodeTypeStep, 'TODO'),
  [StepKind.Member]: makeStep('member', StepFamily.NodeTypeStep, 'All members of complex types, e.g. classes, structs '),
  [StepKind.MetaData]: makeStep('metaData', StepFamily.NodeTypeStep, 'The meta data node'),
  [StepKind.Method]: makeStep('method', StepFamily.NodeTypeStep, 'All methods'),
  [StepKind.MethodRef]: makeStep('methodRef', StepFamily.NodeTypeStep, 'All method references'),
  [StepKind.MethodReturn]: makeStep('methodReturn', StepFamily.NodeTypeStep, 'All formal return parameters'),
  [StepKind.MethodBinding]: makeStep('methodBinding', StepFamily.NodeTypeStep, 'TODO'),
  [StepKind.Modifier]: makeStep('modifier', StepFamily.NodeTypeStep, 'All modifiers '),
  [StepKind.Namespace]: makeStep('namespace', StepFamily.NodeTypeStep, 'All namespaces'),
  [StepKind.NamespaceBlock]: makeStep('namespaceBlock', StepFamily.NodeTypeStep, 'All namespace blocks'),
  [StepKind.Parameter]: makeStep('parameter', StepFamily.NodeTypeStep, 'All parameters'),
  [StepKind.Returns]: makeStep('returns', StepFamily.NodeTypeStep, 'All actual return parameters'),
  [StepKind.TypeDecl]: makeStep('typeDecl', StepFamily.NodeTypeStep, 'All declarations of types'),
  [StepKind.Tag]: makeStep('tag', StepFamily.NodeTypeStep, 'All tags'),
  [StepKind.Typ]: makeStep('typ', StepFamily.NodeTypeStep, 'TODO'),
  [StepKind.Types]: makeStep('types', StepFamily.NodeTypeStep, 'All used types'),

  [StepKind.AliasTypeDecl]: makeStep('aliasTypeDecl', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasTypeFullName]: makeStep('aliasTypeFullName', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasType]: makeStep('aliasType', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasTypeDeclTransitive]: makeStep('aliasTypeDeclTransitive', StepFamily.ComplexStep, 'TODO'),
  [StepKind.AliasTypeTransitive]: makeStep('aliasTypeTransitive', StepFamily.ComplexStep, 'TODO'),
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
  [StepKind.ReferencedTypeDecl]: makeStep('referencedTypeDecl', StepFamily.ComplexStep, 'TODO'),
};

export const PropertyDirectivesConnections = {
  [StepKind.All]: [
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
  ],
  [StepKind.Argument]: [],
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
    { kind: PropertyDirectiveKind.SPID, contextualDescription: null },
    { kind: PropertyDirectiveKind.Version, contextualDescription: null },
  ],
  [StepKind.Method]: [
    { kind: PropertyDirectiveKind.FullName, contextualDescription: null },
    { kind: PropertyDirectiveKind.ID, contextualDescription: null },
    { kind: PropertyDirectiveKind.IsExternal, contextualDescription: null },
    { kind: PropertyDirectiveKind.Label, contextualDescription: null },
    { kind: PropertyDirectiveKind.LineNumber, contextualDescription: null },
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
    { kind: PropertyDirectiveKind.Name, contextualDescription: null },
    { kind: PropertyDirectiveKind.Value, contextualDescription: null },
  ],
  [StepKind.Typ]: [],
  [StepKind.Types]: [
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
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: "The TAG nodes attached to each node in the graph" },
  ],
  [StepKind.Argument]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
  ],
  [StepKind.Call]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: "The methods the calls refer to" },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.IsDynamic, contextualDescription: null },
    { kind: StepKind.IsStatic, contextualDescription: null },
  ],
  [StepKind.Comment]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
  ],
  [StepKind.File]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Namespace, contextualDescription: null },
    { kind: StepKind.NamespaceBlock, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },
  ],
  [StepKind.Identifier]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
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
  ],
  [StepKind.Member]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Modifier, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },
  ],
  [StepKind.MetaData]: [
    // node type steps
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
  ],
  [StepKind.Method]: [
    // node type steps
    { kind: StepKind.Local, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.MethodReturn, contextualDescription: null },
    { kind: StepKind.Modifier, contextualDescription: null },
    { kind: StepKind.Namespace, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.IsPrivate, contextualDescription: null },
    { kind: StepKind.IsProtected, contextualDescription: null },
    { kind: StepKind.IsPublic, contextualDescription: null },
    { kind: StepKind.IsReturn, contextualDescription: null },
    { kind: StepKind.IsStatic, contextualDescription: null },
    { kind: StepKind.IsVirtual, contextualDescription: null },
  ],
  [StepKind.MethodRef]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
  ],
  [StepKind.MethodReturn]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
  ],
  [StepKind.Modifier]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
  ],
  [StepKind.Namespace]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },
  ],
  [StepKind.NamespaceBlock]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.TypeDecl, contextualDescription: null },
  ],
  [StepKind.Parameter]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
    { kind: StepKind.Typ, contextualDescription: null },
  ],
  [StepKind.Returns]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
  ],
  [StepKind.TypeDecl]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
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
  ],
  [StepKind.Tag]: [
    // node type steps
    { kind: StepKind.Call, contextualDescription: null },
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Literal, contextualDescription: null },
    { kind: StepKind.Local, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.MethodReturn, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },
  ],
  [StepKind.Typ]: [],
  [StepKind.Types]: [
    // node type steps
    { kind: StepKind.File, contextualDescription: null },
    { kind: StepKind.Location, contextualDescription: null },
    { kind: StepKind.Method, contextualDescription: null },
    { kind: StepKind.Namespace, contextualDescription: null },
    { kind: StepKind.Parameter, contextualDescription: null },
    { kind: StepKind.Tag, contextualDescription: null },

    // complex steps
    { kind: StepKind.AliasType, contextualDescription: null },
    { kind: StepKind.AliasTypeDecl, contextualDescription: null },
    { kind: StepKind.BaseType, contextualDescription: null },
    { kind: StepKind.BaseTypeTransitive, contextualDescription: null },
    { kind: StepKind.DerivedType, contextualDescription: null },
    { kind: StepKind.DerivedTypeDecl, contextualDescription: null },
    { kind: StepKind.DerivedTypeTransitive, contextualDescription: null },
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


class OQLRef {
  constructor(steps, propertyDirectives, propertyDirectiveConnections, stepConnections) {
    this.steps = steps;

    const stepsInfo = {};

    for (const [ stepKind, step ] of Object.entries(steps)) {
      const singleStepInfo = {
        kind: stepKind,
        family: step.family,
        queryComponent: step.queryComponent,
        description: step.description,
        propertyDirectives: [],
        allowedSteps: []
      };

      const singleStepPropertyDirectiveConnections = propertyDirectiveConnections[stepKind];
      if ( singleStepPropertyDirectiveConnections != null ) {
        singleStepInfo.propertyDirectives = singleStepPropertyDirectiveConnections.map((connection) => {
          const propertyDirective = propertyDirectives[connection.kind];
          const contextualDescription = connection.contextualDescription;
          if ( contextualDescription != null ) {
            propertyDirective.description = contextualDescription;
            return propertyDirective;
          };
          return propertyDirective;
        });
      };

      const singleStepConnections = stepConnections[stepKind];
      if ( singleStepConnections != null && singleStepConnections.length > 0 ) {
        singleStepInfo.allowedSteps = singleStepConnections.map((connection) => {
          const step = steps[connection.kind];
          const contextualDescription = connection.contextualDescription;
          if ( contextualDescription != null ) {
            step.description = contextualDescription;
            return step;
          };
          return step;
        });
      };

      singleStepInfo.nodeTypeSteps = singleStepInfo.allowedSteps.filter((step) => {
        return step.family == StepFamily.NodeTypeStep;
      });

      singleStepInfo.complexSteps = singleStepInfo.allowedSteps.filter((step) => {
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


export const oqlRef = new OQLRef(
  Steps,
  PropertyDirectives,
  PropertyDirectivesConnections,
  StepConnections,
);

export default oqlRef;


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
