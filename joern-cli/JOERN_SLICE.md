# Joern Slice

`JoernSlice` is the entrypoint for `joern-slice` and specifies ways to extract useful subsets of information from the
CPG. Two modes are available:

* **Data-flow**: This is a pretty standard backwards data-flow slicing command that starts at call arguments and slices 
backwards to create a graph of slices. This is interprocedural and the paths are only limited by a depth argument with a
default of 20.
* **Usages**: This targets locals and parameters and traces what calls they make and in which calls they are used. This is
useful for describing how a variable interacts in a procedure.

## Schemas

Each slicer outputs JSON, which allows the result to be ingested by other processes or libraries e.g. NetworkX.

### Data-Flow

This creates a graph with an additional mapping to denote the methods under which each node belongs:

```json
{
  "nodes": [
    {
      "id": "number",
      "label": "string",
      "name": "string",
      "code": "string",
      "typeFullName": "string",
      "lineNumber": "number",
      "columnNumber": "number"
    },
    ...
  ],
  "edges": [
    {
      "src": "number",
      "dst": "number",
      "label": "string"
    }
  ],
  "methodToChildNode": {
    "<method_name_a>": [
      "node_id_1",
      "node_id_2"
    ],
    "<method_name_b>": [],
    ...
  }
}
```

### Usages

The usages slice describes how a variable interacts within its procedure. This is perhaps a more "descriptive" slice
in some ways. The variables are locals and parameters and the referencing identifiers are tracked to find what the
variable calls and what calls it forms an argument of.

```json
{
  "objectSlices": {
    "<method_name_a>": [
      {
        "targetObj": {
          "name": "identifier_name",
          "typeFullName": "identifier_type",
          "literal": "boolean"
        },
        "definedBy": {
          "name": "identifier_name",
          "typeFullName": "identifier_type",
          "literal": "boolean"
        },
        "invokedCalls": [
          {
            "callName": "name_of_call",
            "paramTypes": [
              "type_of_param1",
              "type_of_param2",
              "type_of_paramN"
            ],
            "returnType": "return_type"
          }
        ],
        "argToCalls": [
          [
            {
              "callName": "name_of_call",
              "paramTypes": [
                "type_of_param1",
                "type_of_param2",
                "type_of_paramN"
              ],
              "returnType": "return_type"
            },
            "number_of_argument_targetObj_is_in"
          ]
        ]
      }, 
      ...
    ],
    "<method_name_b>": [
      ...
    ],
    ...
  },
  "userDefinedTypes": [
    {
      "name": "type_name",
      "fields": [
        {
          "name": "field_name",
          "typeFullName": "type of the field",
          "literal": "boolean"
        }
      ],
      "procedures": [
        {
          "callName": "name_of_call",
          "paramTypes": [
            "type_of_param1",
            "type_of_param2",
            "type_of_paramN"
          ],
          "returnType": "return_type"
        }
      ]
    }
  ]
}
```
