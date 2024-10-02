$ion_schema_2_0

type::{
  name: versions,
  type: list,
  element: string
}

type::{
  name: type_name_info,
  type: struct,
  fields: closed::{
    VERSION: { type: string, occurs: required },
    TYPE_NAMES: { type: list, occurs: required, elements: string },
  }
}

type::{
  name: metadata
  type: struct,
  fields: closed::{
    VERSIONS: { type: versions, occurs: required },
    TYPE_NAME_INFO: { type: list, occurs: required, elements: type_name_info },
  }
}