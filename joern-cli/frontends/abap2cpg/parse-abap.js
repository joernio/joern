#!/usr/bin/env node
// Usage: node parse-abap.js [input-path] [output-dir]
//   input-path: ABAP file or directory (default: ./input)
//   output-dir: JSON output directory (default: ./output)

const fs = require('fs');
const path = require('path');
const { Registry, MemoryFile } = require('@abaplint/core');

const inputArg = process.argv[2] || path.join(__dirname, 'input');
const outputDir = process.argv[3] || path.join(__dirname, 'output');

fs.mkdirSync(outputDir, { recursive: true });

const inputStat = fs.statSync(inputArg);
const inputFiles = inputStat.isDirectory()
  ? fs.readdirSync(inputArg).filter(f => f.endsWith('.abap')).map(f => [path.join(inputArg, f), f])
  : [[inputArg, path.basename(inputArg)]];

for (const [filePath, fileName] of inputFiles) {
  try {
    const reg = new Registry();
    reg.addFile(new MemoryFile(fileName, fs.readFileSync(filePath, 'utf8')));
    reg.parse();

    const obj = Array.from(reg.getObjects())[0];
    const file = obj.getSequencedFiles()[0];
    const stmts = file.getStatements();

    const output = {
      file: fileName,
      objectType: obj.getType(),
      statements: stmts.map(s => ({
        type: s.get().constructor.name,
        tokens: s.getTokens().map(t => ({ str: t.getStr() })),
        start: { row: s.getStart().getRow(), col: s.getStart().getCol() },
        end: { row: s.getEnd().getRow(), col: s.getEnd().getCol() }
      })),
      methods: extractMethods(stmts)
    };

    const outPath = path.join(outputDir, fileName.replace(/\.abap$/, '.json'));
    fs.writeFileSync(outPath, JSON.stringify(output, null, 2));
    console.log(`OK ${fileName} -> ${path.basename(outPath)}`);
  } catch (e) {
    console.error(`ERR ${fileName}: ${e.message}`);
  }
}

function extractMethods(stmts) {
  const methods = [];
  let current = null;
  let collecting = false;
  let body = [];

  for (const stmt of stmts) {
    const type = stmt.get().constructor.name;
    const toks = stmt.getTokens().map(t => t.getStr());
    const pos = s => ({ row: s.getRow(), col: s.getCol() });

    if (collecting && (type === 'EndMethod' || type === 'EndForm' || type === 'EndFunction')) {
      if (current) current.body = body;
      collecting = false; body = []; current = null;
    } else if (collecting) {
      body.push({ type, tokens: toks,
        start: pos(stmt.getStart()), end: pos(stmt.getEnd()) });
    }

    if (type === 'MethodDef') {
      const m = {
        type: 'method_definition', name: null,
        isStatic: toks.some(t => t.toUpperCase() === 'CLASS-METHODS'),
        parameters: { importing: [], exporting: [], changing: [], returning: null },
        start: pos(stmt.getStart()), end: pos(stmt.getEnd())
      };
      for (let i = 0; i < toks.length; i++) {
        const t = toks[i].toUpperCase();
        if ((t === 'METHODS' || t === 'CLASS-METHODS') && i + 1 < toks.length) {
          m.name = toks[i + 1]; break;
        }
      }
      let paramType = null, cur = null;
      for (let i = 0; i < toks.length; i++) {
        const t = toks[i].toUpperCase();
        if (['IMPORTING','EXPORTING','CHANGING','RETURNING'].includes(t)) { paramType = t.toLowerCase(); }
        else if (t === 'VALUE' && paramType) {
          const next = toks[i + 1];
          if (next === '(' && i + 2 < toks.length) { cur = { name: toks[i + 2], type: null, isValue: true }; i += 3; }
          else if (next?.startsWith('(') && next.includes(')')) { cur = { name: next.slice(1, next.indexOf(')')), type: null, isValue: true }; i++; }
        }
        else if (paramType && toks[i].startsWith('!')) { cur = { name: toks[i].slice(1), type: null, isValue: false }; }
        else if (cur && t === 'TYPE' && i + 1 < toks.length) {
          let typeName = toks[i + 1];
          if (typeName.toUpperCase() === 'REF' && toks[i + 2]?.toUpperCase() === 'TO' && toks[i + 3]) {
            typeName = 'REF TO ' + toks[i + 3]; i += 2;
          }
          cur.type = typeName;
          if (paramType === 'returning') m.parameters.returning = cur;
          else m.parameters[paramType].push(cur);
          cur = null;
        }
      }
      methods.push(m); current = m;

    } else if (type === 'MethodImplementation') {
      const m = { type: 'method_implementation', name: null, body: null,
        start: pos(stmt.getStart()), end: pos(stmt.getEnd()) };
      for (let i = 0; i < toks.length; i++) {
        if (toks[i].toUpperCase() === 'METHOD' && i + 1 < toks.length) { m.name = toks[i + 1]; break; }
      }
      methods.push(m); current = m; collecting = true; body = [];

    } else if (type === 'Form') {
      const m = { type: 'form', name: null,
        parameters: { using: [], changing: [], tables: [] }, body: null,
        start: pos(stmt.getStart()), end: pos(stmt.getEnd()) };
      let paramType = null;
      for (let i = 0; i < toks.length; i++) {
        const t = toks[i].toUpperCase();
        if (t === 'FORM' && i + 1 < toks.length) m.name = toks[i + 1];
        else if (['USING','CHANGING','TABLES'].includes(t)) paramType = t.toLowerCase();
        else if (paramType && !['FORM','USING','CHANGING','TABLES','TYPE'].includes(t))
          m.parameters[paramType].push({ name: toks[i], type: null });
      }
      methods.push(m); current = m; collecting = true; body = [];

    } else if (type === 'FunctionModule') {
      const m = { type: 'function_module', name: null, body: null,
        start: pos(stmt.getStart()), end: pos(stmt.getEnd()) };
      for (let i = 0; i < toks.length; i++) {
        if (toks[i].toUpperCase() === 'FUNCTION' && i + 1 < toks.length) { m.name = toks[i + 1]; break; }
      }
      methods.push(m); current = m; collecting = true; body = [];
    }
  }
  return methods;
}
