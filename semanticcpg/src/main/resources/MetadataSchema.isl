$ion_schema_2_0

type::{
  name: versions,
  type: list,
  element: string
}

type::{
  name: metadata
  type: struct,
  fields: closed::{
    VERSIONS: { type: versions, occurs: required },
  }
}