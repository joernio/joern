#!/usr/bin/php

<?php
// https://github.com/hugowetterberg/php-class-parser
class ClassParser
{
  private $classes = array();
  private $extends = array();
  private $implements = array();

  const STATE_CLASS_HEAD = 100001;
  const STATE_FUNCTION_HEAD = 100002;

  public function getClasses()
  {
    return array_values($this->classes);
  }

  public function getClassesImplementing($interface)
  {
    $implementers = array();
    if (isset($this->implements[$interface])) {
      foreach ($this->implements[$interface] as $name) {
        $implementers[$name] = $this->classes[$name];
      }
    }
    return $implementers;
  }

  public function getClassesExtending($class)
  {
    $extenders = array();
    if (isset($this->extends[$class])) {
      foreach ($this->extends[$class] as $name) {
        $extenders[$name] = $this->classes[$name];
      }
    }
    return $extenders;
  }

  public function parse($file, $parent_dir)
  {
    $file = realpath($file);
    $tokens = token_get_all(file_get_contents($file));
    $classes = array();

    $si = NULL;
    $depth = 0;
    $mod = array();
    $state = NULL;

    foreach ($tokens as $idx => &$token) {
      if (is_array($token)) {
        switch ($token[0]) {
          case T_DOC_COMMENT:
            break;
          case T_PUBLIC:
          case T_PRIVATE:
          case T_STATIC:
          case T_ABSTRACT:
          case T_PROTECTED:
            $mod[] = $token[1];
            break;
          case T_CLASS:
          case T_FUNCTION:
            $state = $token[0];
            break;
          case T_EXTENDS:
          case T_IMPLEMENTS:
            switch ($state) {
              case self::STATE_CLASS_HEAD:
              case T_EXTENDS:
                $state = $token[0];
                break;
            }
            break;
          case T_STRING:
            switch ($state) {
              case T_CLASS:
                $state = self::STATE_CLASS_HEAD;
                $si = $token[1];
                $classes[] = array('name' => $token[1], 'modifiers' => $mod);
                break;
              case T_FUNCTION:
                $state = self::STATE_FUNCTION_HEAD;
                $clsc = count($classes);
                if ($depth > 0 && $clsc) {
                  $classes[$clsc - 1]['functions'][$token[1]] = array('modifiers' => $mod);
                }
                break;
              case T_IMPLEMENTS:
              case T_EXTENDS:
                $clsc = count($classes);
                $classes[$clsc - 1][$state == T_IMPLEMENTS ? 'implements' : 'extends'][] = $token[1];
                break;
            }
            break;
        }
      } else {
        switch ($token) {
          case '{':
            $depth++;
            break;
          case '}':
            $depth--;
            break;
        }

        switch ($token) {
          case '{':
          case '}':
          case ';':
            $state = 0;
            $mod = array();
            break;
        }
      }
    }

    foreach ($classes as $class) {
      $class['file'] = ltrim(str_replace($parent_dir, "", $file), "\\/");
      $class['namespace'] = implode("\\", array_filter(explode(DIRECTORY_SEPARATOR, ltrim(str_replace($parent_dir, "", dirname($file)), "\\/")), fn ($value) => $value !== ""));
      $this->classes[$class['name']] = $class;

      if (!empty($class['implements'])) {
        foreach ($class['implements'] as $name) {
          $this->implements[$name][] = $class['name'];
        }
      }

      if (!empty($class['extends'])) {
        foreach ($class['extends'] as $name) {
          $this->extends[$name][] = $class['name'];
        }
      }
    }
  }
}
?>

<?php

if ($argc != 2 || in_array($argv[1], array('--help', '-help', '-h', '-?'))) {
?>

  This is a command line PHP script that parses the symbol
  information from a given input directory.

  Usage:
  <?php echo $argv[0]; ?> <input-dir>

    <input-dir> the input directory to parse.
      to print out. With the --help, -help, -h,
      or -? options, you can get this help.

    <?php
  } else {
    $target_dir = realpath($argv[1]);
    if (!is_dir($target_dir)) {
      echo $target_dir . " is an invalid directory!";
    }
    $cp = new ClassParser();
    $it = new RecursiveDirectoryIterator($target_dir);

    // Loop through files
    foreach (new RecursiveIteratorIterator($it) as $file) {
      if ($file->getExtension() === 'php') {
        $cp->parse($file, $target_dir);
      }
    }
    echo json_encode($cp->getClasses());
  }
    ?>