$ion_schema_2_0

type::{
  name: full_name,
  type: string
}

type::{
  name: name,
  type: string
}

type::{
  name: version,
  type: string
}

type::{
  name: signature,
  type: string
}

type::{
  name: type_parameters,
  type: list,
  element: name
}

type::{
  name: inherits,
  type: list,
  element: full_name
}

type::{
  name: method,
  type: struct,
  fields: closed::{
    NAME: { type: name, occurs: required },
    FULL_NAME: { type: full_name, occurs: required },
    SIGNATURE: { type: signature, occurs: required }
  }  
}

type::{
  name: methods,
  type: list,
  element: method
}

type::{
  name: member,
  type: struct,
  fields: closed::{
    NAME: { type: name, occurs: required },
    TYPE_FULL_NAME: { type: full_name, occurs: required }
  }
}

type::{
  name: members,
  type: list,
  element: member
}

type::{
  name: type_decl,
  type: struct,
  fields: closed::{
    FULL_NAME: { type: full_name, occurs: required },
    NAME: { type: name, occurs: required },
    TYPE_PARAMETERS: type_parameters,
    INHERITS: inherits,
    METHODS: methods,
    MEMBERS: members
  }
}
