#!/usr/bin/env node
// Usage: parse-abap.js <input-dir> <output-dir>
// Dumps raw @abaplint/core statements as JSON. All interpretation in AbapJsonParser.scala.

const fs = require('fs');
const path = require('path');
const { Registry, MemoryFile } = require('@abaplint/core');

const [,, inputArg, outputDir] = process.argv;
if (!inputArg || !outputDir) { process.stderr.write('Usage: parse-abap.js <input-dir> <output-dir>\n'); process.exit(1); }

fs.mkdirSync(outputDir, { recursive: true });

const pairs = fs.statSync(inputArg).isDirectory()
  ? fs.readdirSync(inputArg).filter(f => f.endsWith('.abap')).map(f => [path.join(inputArg, f), f])
  : [[inputArg, path.basename(inputArg)]];

for (const [absPath, relName] of pairs) {
  try {
    const reg = new Registry();
    reg.addFile(new MemoryFile(relName, fs.readFileSync(absPath, 'utf8')));
    reg.parse();

    const obj  = [...reg.getObjects()][0];
    const file = obj && (obj.getSequencedFiles ? obj.getSequencedFiles()[0] : obj.getFiles()[0]);
    if (!obj || !file) { process.stdout.write(`ERR ${absPath}\n`); continue; }

    const statements = file.getStatements().map(s => ({
      type:   s.get().constructor.name,
      tokens: s.getTokens().map(t => ({ str: t.getStr() })),
      start:  { row: s.getStart().getRow(), col: s.getStart().getCol() },
      end:    { row: s.getEnd().getRow(),   col: s.getEnd().getCol() }
    }));

    const outPath = path.join(outputDir, relName.replace(/\.abap$/, '.json'));
    fs.writeFileSync(outPath, JSON.stringify({ file: relName, objectType: obj.getType(), statements }));
    process.stdout.write(`OK ${outPath}\n`);
  } catch (e) {
    process.stderr.write(`Error: ${absPath}: ${e.message}\n`);
    process.stdout.write(`ERR ${absPath}\n`);
  }
}
