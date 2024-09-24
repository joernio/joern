$ion_schema_2_0

type::{
  name: name,
  type: string
}

type::{
  name: constraint,
  type: string
}

type::{
  name: lang,
  type: string
}

type::{
  name: dependency,
  type: struct,
  fields: closed::{
    NAME: { type: name, occurs: required },
    CONSTRAINT: { type: constraint, occurs: required },
    LANG: { type: lang }
  }
}

type::{
  name: dependency_list,
  type: list,
  element: dependency
}