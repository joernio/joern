<?php
/**
 * Dependency symbol extractor for the PHP frontend.
 *
 * Usage:  php ClassParser.php <dependency-root>
 *
 * Walks all `.php` files below <dependency-root>, finds every named class / interface / trait / enum declaration, and
 * prints ONE line of JSON on stdout: an array of type records
 *
 *   {"name","namespace","file","kind","modifiers","extends","implements","functions"?}
 *
 * `functions` maps a method name to {"modifiers": [...]} and is omitted when the type has no methods. Diagnostics go
 * to stderr only. The input is never executed, only tokenized (tokenizer extension, no parsing of whole files).
 *
 * Design: every file is tokenized once. Whitespace, comments and inline HTML are dropped, then a single pass over the
 * remaining tokens runs a small state machine (see FileScanner). The states are:
 *
 *   SCAN          main loop. Tracks a stack of brace frames (BLOCK / TYPE / NAMESPACE) and the pending modifiers.
 *   TYPE_HEADER   after `class|interface|trait|enum Name`: reads `extends`, `implements`, enum backing type, up to `{`.
 *   METHOD_HEADER after `function` directly inside a TYPE frame: reads the name and skips parameters and return type.
 *   NAMESPACE     after `namespace`: reads the name, then `;` or `{`.
 *   ATTRIBUTE     `#[ ... ]` (PHP 8 only, PHP 7 sees a comment): skipped completely.
 *
 * Everything that opens a brace pushes a frame, so methods are only recorded when the innermost frame is a TYPE frame.
 * Method bodies, closures, anonymous class bodies, `{$x}` / `${x}` string interpolation and trait adaptation blocks
 * all push a BLOCK frame, which keeps nesting right and keeps nested functions out of the method list.
 *
 * NOTE: `short_open_tag` cannot be changed at runtime and the tokenizer follows it. Files using `<?` instead of
 * `<?php` are therefore only understood when the PHP in use has `short_open_tag=1`. The behaviour on such files
 * depends on that setting.
 */

// ---- Tunables ----------------------------------------------------------------------------------------------------

/** Maximum number of diagnostic lines written to stderr. Further ones are only counted. */
const MAX_DIAGNOSTIC_LINES = 100;

/** Directory names (matched case-insensitively, at any depth) that are not scanned. Each skip is reported on stderr. */
const EXCLUDED_DIRECTORIES = ['tests', 'examples'];

/**
 * Files bigger than this are skipped, because PHP cannot recover from running out of memory while tokenizing. A token
 * costs well over 100 bytes, so this keeps one file below roughly 500 MB with MEMORY_LIMIT in effect.
 */
const MAX_FILE_BYTES = 4194304;

/** Memory limit the script asks for. It processes one file at a time. */
const MEMORY_LIMIT = '1024M';

// ---- Environment hardening ---------------------------------------------------------------------------------------

// Nothing but the final JSON may reach stdout, whatever php.ini says (notices and warnings go to stdout by default).
error_reporting(0);
@ini_set('display_errors', '0');
@ini_set('html_errors', '0');
@ini_set('memory_limit', MEMORY_LIMIT);
@set_time_limit(0);

register_shutdown_function(function () {
    $error = error_get_last();
    if ($error !== null && in_array($error['type'], [E_ERROR, E_CORE_ERROR, E_COMPILE_ERROR, E_PARSE], true)) {
        fwrite(STDERR, 'Fatal error: ' . $error['message'] . "\n");
    }
});

// ---- Diagnostics -------------------------------------------------------------------------------------------------

final class Diagnostics
{
    private $written = 0;
    private $suppressed = 0;

    public function report(string $message): void
    {
        if ($this->written < MAX_DIAGNOSTIC_LINES) {
            $this->written++;
            fwrite(STDERR, str_replace(["\r", "\n"], ' ', $message) . "\n");
        } else {
            $this->suppressed++;
        }
    }

    /** Written last, does not count against the cap. */
    public function summary(int $parsed, int $considered): void
    {
        if ($this->suppressed > 0) {
            fwrite(STDERR, $this->suppressed . " further messages suppressed.\n");
        }
        fwrite(STDERR, "Parsed $parsed/$considered files successfully.\n");
    }
}

// ---- Per-file scanner --------------------------------------------------------------------------------------------

final class FileScanner
{
    /** Pseudo token id for single character tokens such as `{` (the tokenizer returns them as plain strings). */
    const CHAR = -2;

    const FRAME_BLOCK = 0;      // any brace pair that is not a type body or namespace block
    const FRAME_TYPE = 1;       // body of a named class, interface, trait or enum
    const FRAME_NAMESPACE = 2;  // `namespace X { ... }`

    /** Token ids by name. Tokens a PHP version does not know get -1, which never matches a real token. */
    private $T = [];
    /** Set of token ids that can be part of a (qualified) name. */
    private $nameIds = [];
    /** Set of token ids that are declaration modifiers. */
    private $modifierIds = [];
    /** Set of token ids dropped before scanning. */
    private $ignoredIds = [];

    // State of the file being scanned. Reset by scan(), so nothing leaks from one file to the next.
    private $ids = [];
    private $txt = [];
    private $count = 0;
    private $records = [];
    private $stack = [];
    private $namespace = '';

    public function __construct()
    {
        foreach ([
            'T_STRING', 'T_CLASS', 'T_INTERFACE', 'T_TRAIT', 'T_ENUM', 'T_FUNCTION', 'T_NAMESPACE', 'T_NEW',
            'T_EXTENDS', 'T_IMPLEMENTS', 'T_NS_SEPARATOR', 'T_NAME_QUALIFIED', 'T_NAME_FULLY_QUALIFIED',
            'T_NAME_RELATIVE', 'T_DOUBLE_COLON', 'T_OBJECT_OPERATOR', 'T_NULLSAFE_OBJECT_OPERATOR', 'T_ATTRIBUTE',
            'T_CURLY_OPEN', 'T_DOLLAR_OPEN_CURLY_BRACES', 'T_HALT_COMPILER', 'T_OPEN_TAG', 'T_OPEN_TAG_WITH_ECHO',
            'T_CLOSE_TAG', 'T_ELSE', 'T_PUBLIC', 'T_PROTECTED', 'T_PRIVATE', 'T_STATIC', 'T_ABSTRACT', 'T_FINAL',
            'T_READONLY', 'T_WHITESPACE', 'T_COMMENT', 'T_DOC_COMMENT', 'T_INLINE_HTML',
        ] as $name) {
            $this->T[$name] = defined($name) ? constant($name) : -1;
        }
        $T = $this->T;
        foreach (['T_STRING', 'T_NS_SEPARATOR', 'T_NAME_QUALIFIED', 'T_NAME_FULLY_QUALIFIED', 'T_NAME_RELATIVE',
                  'T_NAMESPACE'] as $name) {
            if ($T[$name] !== -1) {
                $this->nameIds[$T[$name]] = true;
            }
        }
        foreach (['T_PUBLIC', 'T_PROTECTED', 'T_PRIVATE', 'T_STATIC', 'T_ABSTRACT', 'T_FINAL', 'T_READONLY'] as $name) {
            if ($T[$name] !== -1) {
                $this->modifierIds[$T[$name]] = true;
            }
        }
        foreach (['T_WHITESPACE', 'T_COMMENT', 'T_DOC_COMMENT', 'T_INLINE_HTML'] as $name) {
            $this->ignoredIds[$T[$name]] = true;
        }
    }

    /**
     * @return array[] type records of the file, in order of declaration
     * @throws Exception when the file cannot be reported (for example invalid UTF-8 in a name)
     */
    public function scan(string $source, string $file): array
    {
        $this->tokenize($source);
        $this->records = [];
        $this->stack = [];
        $this->namespace = '';

        $T = $this->T;
        $ids = $this->ids;
        $txt = $this->txt;
        $n = $this->count;
        $modifierIds = $this->modifierIds;

        $mods = [];           // modifiers seen directly before the current token, source order, lower case
        $prevId = -1;         // previous token (attributes excluded)
        $prevTxt = '';
        $pendingNew = false;  // true right after `new` (and `new readonly`): a following `class` is anonymous

        for ($i = 0; $i < $n; $i++) {
            $id = $ids[$i];
            $t = $txt[$i];

            if ($id === $T['T_ATTRIBUTE']) {
                $i = $this->skipAttribute($i);
                continue;
            }
            if ($id === $T['T_HALT_COMPILER']) {
                break;
            }

            $keepNew = false;
            $lower = strtolower($t);

            if ($id === self::CHAR) {
                if ($t === '{') {
                    $this->push(self::FRAME_BLOCK, -1);
                } elseif ($t === '}') {
                    $this->pop();
                }
                $mods = [];
            } elseif ($id === $T['T_CURLY_OPEN'] || $id === $T['T_DOLLAR_OPEN_CURLY_BRACES']) {
                // `{$x}` and `${x}` in strings. The matching `}` arrives as a bare `}`.
                $this->push(self::FRAME_BLOCK, -1);
                $mods = [];
            } elseif (isset($modifierIds[$id]) || ($id === $T['T_STRING'] && $lower === 'readonly')) {
                // `readonly` is a plain T_STRING before PHP 8.1. It only matters if a declaration follows at once.
                $mods[] = $lower;
                $keepNew = ($lower === 'readonly');
            } elseif ($id === $T['T_FUNCTION']) {
                if ($this->topFrameKind() === self::FRAME_TYPE) {
                    $i = $this->parseMethod($i, $mods);
                }
                $mods = [];
            } elseif ($id === $T['T_NEW']) {
                $mods = [];
            } elseif ($id === $T['T_CLASS'] || $id === $T['T_INTERFACE'] || $id === $T['T_TRAIT']) {
                $isMemberAccess = $prevId === $T['T_DOUBLE_COLON'] || $prevId === $T['T_OBJECT_OPERATOR']
                    || $prevId === $T['T_NULLSAFE_OBJECT_OPERATOR'];
                $isAnonymous = $id === $T['T_CLASS'] && $pendingNew;
                if (!$isMemberAccess && !$isAnonymous) {
                    $kind = $id === $T['T_CLASS'] ? 'class' : ($id === $T['T_INTERFACE'] ? 'interface' : 'trait');
                    $i = $this->parseTypeDeclaration($kind, $i, $mods, $file);
                }
                $mods = [];
            } elseif (($id === $T['T_ENUM'] || $id === $T['T_STRING']) && $lower === 'enum'
                && $this->startsStatement($prevId, $prevTxt)) {
                $i = $this->parseTypeDeclaration('enum', $i, $mods, $file);
                $mods = [];
            } elseif ($id === $T['T_NAMESPACE']) {
                $i = $this->parseNamespace($i);
                $mods = [];
            } else {
                $mods = [];
            }

            $pendingNew = ($id === $T['T_NEW']) || ($pendingNew && $keepNew);
            $prevId = $ids[$i];
            $prevTxt = $txt[$i];
        }

        $this->ids = [];
        $this->txt = [];
        return $this->finish($file);
    }

    // ---- Tokenizing ----------------------------------------------------------------------------------------------

    private function tokenize(string $source): void
    {
        $raw = token_get_all($source);
        $ids = [];
        $txt = [];
        foreach ($raw as $token) {
            if (is_array($token)) {
                if (isset($this->ignoredIds[$token[0]])) {
                    continue;
                }
                $ids[] = $token[0];
                $txt[] = $token[1];
            } else {
                $ids[] = self::CHAR;
                $txt[] = $token;
            }
        }
        unset($raw);
        $this->ids = $ids;
        $this->txt = $txt;
        $this->count = count($ids);
    }

    private function isIdentifier(int $k): bool
    {
        return $k < $this->count && $this->ids[$k] !== self::CHAR
            && preg_match('/^[A-Za-z_\x80-\xff][A-Za-z0-9_\x80-\xff]*$/D', $this->txt[$k]) === 1;
    }

    private function isNamePart(int $k): bool
    {
        return $k < $this->count && isset($this->nameIds[$this->ids[$k]]);
    }

    private function isChar(int $k, string $char): bool
    {
        return $k < $this->count && $this->ids[$k] === self::CHAR && $this->txt[$k] === $char;
    }

    /** True if `enum` after this token can start a declaration. */
    private function startsStatement(int $prevId, string $prevTxt): bool
    {
        if ($prevId === -1) {
            return true;
        }
        if ($prevId === self::CHAR) {
            return in_array($prevTxt, [';', '{', '}', ':', ')'], true);
        }
        return $prevId === $this->T['T_OPEN_TAG'] || $prevId === $this->T['T_OPEN_TAG_WITH_ECHO']
            || $prevId === $this->T['T_CLOSE_TAG'] || $prevId === $this->T['T_ELSE'];
    }

    // ---- Frames --------------------------------------------------------------------------------------------------

    private function push(int $kind, int $record): void
    {
        $this->stack[] = [$kind, $record];
    }

    private function pop(): void
    {
        $frame = array_pop($this->stack);
        if ($frame !== null && $frame[0] === self::FRAME_NAMESPACE) {
            $this->namespace = '';
        }
    }

    private function topFrameKind(): int
    {
        $top = end($this->stack);
        return $top === false ? -1 : $top[0];
    }

    // ---- State ATTRIBUTE -----------------------------------------------------------------------------------------

    /** @return int index of the `]` that closes the attribute starting at $i */
    private function skipAttribute(int $i): int
    {
        $depth = 0;
        for ($k = $i; $k < $this->count; $k++) {
            if ($this->ids[$k] === $this->T['T_ATTRIBUTE'] || $this->isChar($k, '[')) {
                $depth++;
            } elseif ($this->isChar($k, ']')) {
                $depth--;
                if ($depth === 0) {
                    return $k;
                }
            }
        }
        return $this->count - 1;
    }

    // ---- State NAMESPACE -----------------------------------------------------------------------------------------

    /** @return int index of the last token consumed */
    private function parseNamespace(int $i): int
    {
        $k = $i + 1;
        if ($k < $this->count && $this->ids[$k] === $this->T['T_NS_SEPARATOR']) {
            return $i; // PHP 7 `namespace\foo()`: relative name, not a declaration
        }
        $name = '';
        while ($this->isNamePart($k)) {
            $name .= $this->txt[$k];
            $k++;
        }
        $terminatesByBrace = $this->isChar($k, '{');
        if ($terminatesByBrace || $this->isChar($k, ';') || ($k < $this->count && $this->ids[$k] === $this->T['T_CLOSE_TAG'])) {
            $this->namespace = trim($name, '\\');
            if ($terminatesByBrace) {
                $this->push(self::FRAME_NAMESPACE, -1);
            }
            return $k;
        }
        return $i;
    }

    // ---- State TYPE_HEADER ---------------------------------------------------------------------------------------

    /** @return int index of the last token consumed (the `{` of the body when there is one) */
    private function parseTypeDeclaration(string $kind, int $i, array $mods, string $file): int
    {
        $nameIdx = $i + 1;
        if (!$this->isIdentifier($nameIdx) || $this->ids[$nameIdx] !== $this->T['T_STRING']) {
            return $i;
        }
        if ($kind === 'enum') {
            $next = $nameIdx + 1;
            if (!($this->isChar($next, '{') || $this->isChar($next, ':')
                || ($next < $this->count && $this->ids[$next] === $this->T['T_IMPLEMENTS']))) {
                return $i; // `enum Foo` is not followed by a body, so it is not a declaration
            }
        }

        $record = [
            'name' => $this->txt[$nameIdx],
            'namespace' => $this->namespace,
            'file' => $file,
            'kind' => $kind,
            'modifiers' => array_values(array_filter($mods, function ($m) {
                return $m === 'abstract' || $m === 'final' || $m === 'readonly';
            })),
            'extends' => [],
            'implements' => [],
            'functions' => [],
            'seen' => [],
        ];
        $this->records[] = $record;
        $recordIdx = count($this->records) - 1;

        $list = null;   // 'extends', 'implements' or null (nothing collected, e.g. enum backing type)
        $buffer = '';
        $T = $this->T;
        for ($k = $nameIdx + 1; $k < $this->count; $k++) {
            $id = $this->ids[$k];
            $isBrace = $this->isChar($k, '{');
            $isSemicolon = $this->isChar($k, ';');
            if ($isBrace || $isSemicolon || $id === $T['T_EXTENDS'] || $id === $T['T_IMPLEMENTS']
                || $this->isChar($k, ',') || $this->isChar($k, ':')) {
                if ($list !== null && $buffer !== '') {
                    $this->records[$recordIdx][$list][] = $buffer;
                }
                $buffer = '';
                if ($isBrace) {
                    $this->push(self::FRAME_TYPE, $recordIdx);
                    return $k;
                }
                if ($isSemicolon) {
                    return $k;
                }
                if ($id === $T['T_EXTENDS']) {
                    $list = 'extends';
                } elseif ($id === $T['T_IMPLEMENTS']) {
                    $list = 'implements';
                } elseif ($this->isChar($k, ':')) {
                    $list = null;
                }
            } elseif ($id === $T['T_ATTRIBUTE']) {
                $k = $this->skipAttribute($k);
            } elseif ($list !== null && $this->isNamePart($k)) {
                $buffer .= $this->txt[$k];
            }
        }
        return $this->count - 1;
    }

    // ---- State METHOD_HEADER -------------------------------------------------------------------------------------

    /** @return int index of the token before the `{` or `;` that ends the header */
    private function parseMethod(int $i, array $mods): int
    {
        $k = $i + 1;
        if ($this->isChar($k, '&') || ($k < $this->count && $this->txt[$k] === '&')) {
            $k++; // return by reference (PHP 8.1 has dedicated token ids for `&`)
        }
        if (!$this->isIdentifier($k)) {
            return $i;
        }
        $name = $this->txt[$k];
        $k++;
        if (!$this->isChar($k, '(')) {
            return $i;
        }
        $depth = 0;
        for (; $k < $this->count; $k++) {
            if ($this->isChar($k, '(')) {
                $depth++;
            } elseif ($this->isChar($k, ')')) {
                $depth--;
                if ($depth === 0) {
                    break;
                }
            }
        }
        // return type, then `{` or `;`
        for ($k++; $k < $this->count; $k++) {
            if ($this->isChar($k, '{') || $this->isChar($k, ';')) {
                break;
            }
        }

        $top = end($this->stack);
        $key = strtolower($name);
        if (!isset($this->records[$top[1]]['seen'][$key])) {
            $this->records[$top[1]]['seen'][$key] = true;
            $this->records[$top[1]]['functions'][$name] = [
                'modifiers' => array_values(array_filter($mods, function ($m) {
                    return $m !== 'readonly';
                })),
            ];
        }
        return $k - 1;
    }

    // ---- Result --------------------------------------------------------------------------------------------------

    private function finish(string $file): array
    {
        $out = [];
        foreach ($this->records as $record) {
            $result = [
                'name' => $record['name'],
                'namespace' => $record['namespace'],
                'file' => $record['file'],
                'kind' => $record['kind'],
                'modifiers' => $record['modifiers'],
                'extends' => $record['extends'],
                'implements' => $record['implements'],
            ];
            if (count($record['functions']) > 0) {
                $result['functions'] = $record['functions'];
            }

            $strings = array_merge(
                [$record['name'], $record['namespace']],
                $record['extends'],
                $record['implements'],
                array_map('strval', array_keys($record['functions']))
            );
            foreach ($strings as $s) {
                if (preg_match('//u', $s) !== 1) {
                    throw new Exception('invalid UTF-8 in a declared name');
                }
            }
            $out[] = $result;
        }
        $this->records = [];
        return $out;
    }
}

// ---- Directory walk ----------------------------------------------------------------------------------------------

/**
 * @return string[] relative paths ('/' separated) of the .php files below $root, sorted bytewise.
 *                  Symlinks are not followed. Excluded directories are not entered.
 */
function collectPhpFiles(string $root, Diagnostics $diagnostics): array
{
    $files = [];
    $pending = [''];
    while (count($pending) > 0) {
        $relativeDir = array_pop($pending);
        $dir = $relativeDir === '' ? $root : $root . '/' . $relativeDir;
        $handle = @opendir($dir);
        if ($handle === false) {
            $diagnostics->report("Skipping unreadable directory: $dir");
            continue;
        }
        while (($entry = readdir($handle)) !== false) {
            if ($entry === '.' || $entry === '..') {
                continue;
            }
            $path = $dir . '/' . $entry;
            $relative = $relativeDir === '' ? $entry : $relativeDir . '/' . $entry;
            if (is_link($path)) {
                continue;
            }
            if (is_dir($path)) {
                if (in_array(strtolower($entry), EXCLUDED_DIRECTORIES, true)) {
                    $diagnostics->report("Skipping excluded directory: $relative");
                } else {
                    $pending[] = $relative;
                }
            } elseif (is_file($path) && strlen($entry) >= 4 && strtolower(substr($entry, -4)) === '.php') {
                $files[] = $relative;
            }
        }
        closedir($handle);
    }
    sort($files, SORT_STRING);
    return $files;
}

// ---- Main --------------------------------------------------------------------------------------------------------

function runClassParser(array $argv): int
{
    $args = array_slice($argv, 1);
    if (count($args) !== 1 || in_array($args[0], ['--help', '-help', '-h', '-?'], true)) {
        fwrite(STDERR, "Usage: php ClassParser.php <dependency-root>\n"
            . "Prints a JSON array with one record per class, interface, trait and enum below <dependency-root>.\n");
        return 2;
    }

    $root = realpath($args[0]);
    if ($root === false || !is_dir($root)) {
        fwrite(STDERR, 'Dependency root does not exist or is not a directory: ' . $args[0] . "\n");
        return 1;
    }
    $probe = @opendir($root);
    if ($probe === false) {
        fwrite(STDERR, 'Dependency root cannot be read: ' . $args[0] . "\n");
        return 1;
    }
    closedir($probe);

    $diagnostics = new Diagnostics();
    $files = collectPhpFiles($root, $diagnostics);
    $scanner = new FileScanner();
    $records = [];
    $parsed = 0;

    foreach ($files as $relative) {
        if (preg_match('//u', $relative) !== 1) {
            $diagnostics->report("Skipping file with a path that is not valid UTF-8: $relative");
            continue;
        }
        $path = $root . '/' . $relative;
        $size = @filesize($path);
        if ($size !== false && $size > MAX_FILE_BYTES) {
            $diagnostics->report("Skipping $relative: file is larger than " . MAX_FILE_BYTES . ' bytes');
            continue;
        }
        $source = @file_get_contents($path);
        if ($source === false) {
            $diagnostics->report("Skipping $relative: cannot be read");
            continue;
        }
        try {
            $fileRecords = $scanner->scan($source, $relative);
        } catch (Throwable $e) {
            $diagnostics->report("Skipping $relative: " . $e->getMessage());
            continue;
        }
        $parsed++;
        foreach ($fileRecords as $record) {
            $records[] = $record;
        }
        unset($source, $fileRecords);
    }

    // TODO: check this encoding choice. Non-ASCII characters and `/` are written raw; verify how the frontend decodes
    // non-ASCII output on platforms whose default charset is not UTF-8.
    $json = json_encode($records, JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE);
    if ($json === false) {
        fwrite(STDERR, 'Cannot encode the result as JSON: ' . json_last_error_msg() . "\n");
        $diagnostics->summary($parsed, count($files));
        return 1;
    }
    fwrite(STDOUT, $json . "\n");
    $diagnostics->summary($parsed, count($files));
    return 0;
}

try {
    $arguments = isset($argv) ? $argv : (isset($_SERVER['argv']) ? $_SERVER['argv'] : []);
    exit(runClassParser($arguments));
} catch (Throwable $e) {
    fwrite(STDERR, 'ClassParser failed: ' . $e->getMessage() . "\n");
    exit(1);
}
