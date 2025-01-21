# Support For New Language Features

- For an explanation for each feature you may want to look at https://github.com/AnthonyCalandra/modern-cpp-features/tree/master.
- Table legend:
  - `[?]` not yet checked
  - `[ ]` not supported at all / can not even be parsed
  - `[~]` can be parsed but is not fully represented in the CPG
  - `[x]` full support including the CPG representation

## C++17 Language Features

| Feature                                                                 | Supported |
|-------------------------------------------------------------------------|-----------|
| template argument deduction for class templates                         | [~]       |
| declaring non-type template parameters with auto                        | [~]       |
| folding expressions                                                     | [x]       |
| new rules for auto deduction from braced-init-list                      | [x]       |
| constexpr lambda                                                        | [~]       |
| lambda capture this by value                                            | [~]       |
| inline variables                                                        | [x]       |
| nested namespaces                                                       | [x]       |
| structured bindings                                                     | [x]       |
| selection statements with initializer                                   | [x]       |
| constexpr if                                                            | [x]       |
| utf-8 character literals                                                | [ ]       |
| direct-list-initialization of enums                                     | [x]       |
| \[\[fallthrough\]\], \[\[nodiscard\]\], \[\[maybe_unused\]\] attributes | [~]       |
| \_\_has_include                                                         | [~]       |
| class template argument deduction                                       | [~]       |

## C++20 Language Features

| Feature                                        | Supported |
|------------------------------------------------|-----------|
| coroutines                                     | [~]       |
| concepts                                       | [ ]       |
| three-way comparison                           | [ ]       |
| designated initializers                        | [~]       |
| template syntax for lambdas                    | [ ]       |
| range-based for loop with initializer          | [ ]       |
| \[\[likely\]\] and \[\[unlikely\]\] attributes | [x]       |
| deprecate implicit capture of this             | [~]       |
| class types in non-type template parameters    | [~]       |
| constexpr virtual functions                    | [~]       |
| explicit(bool)                                 | [ ]       |
| immediate functions                            | [x]       |
| using enum                                     | [ ]       |
| lambda capture of parameter pack               | [ ]       |
| char8_t                                        | [x]       |
| constinit                                      | [+]       |
| \_\_VA_OPT\_\_                                 | [ ]       |
