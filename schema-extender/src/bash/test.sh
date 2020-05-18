#!/bin/bash
set -e #stop on error

echo "testing schema extender"

# assuming we are in ocular installation
set -x #verbose on
cd schema-extender
echo '{"nodeTypes" : [{ "id": 9876, "name":"FOO", "outEdges":[], "keys":["CODE"] }]}' > schemas/foo.json
bin/schema-extender
# ocular should now have the new schema and we should be able to use our new `Foo` node
cd -
echo 'assert(nodes.Foo.Label == "FOO")
assert(nodes.Foo.PropertyNames.all.contains("CODE"))' > scripts/SchemaExtenderTest.sc
./ocular.sh --script scripts/SchemaExtenderTest.sc
