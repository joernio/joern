package io.joern.swiftsrc2cpg.passes

object GlobalBuiltins {

  // TODO: this is still incomplete
  val builtins: Set[String] = Set(
    // Input/Output Functions
    "print",      // Prints values to standard output
    "debugPrint", // Prints debug description of values
    "dump",       // Dumps detailed object structure information

    // Type Conversion Functions
    "String",     // Converts a value to a string
    "Int",        // Converts a value to an integer
    "Double",     // Converts a value to a double
    "Float",      // Converts a value to a float
    "Bool",       // Converts a value to a boolean
    "Array",      // Creates an array from a sequence
    "Dictionary", // Creates a dictionary from a sequence of key-value pairs
    "Set",        // Creates a set from a sequence

    // Collection Functions
    "min",        // Returns the minimum element in a sequence
    "max",        // Returns the maximum element in a sequence
    "sorted",     // Returns a sorted array of the sequence's elements
    "filter",     // Returns an array containing elements that satisfy a predicate
    "map",        // Returns an array containing the results of mapping a transform over the sequence
    "reduce",     // Returns the result of combining the elements using a closure
    "flatMap",    // Returns an array containing the concatenated results of calling a transform
    "compactMap", // Returns an array containing the non-nil results of calling a transform
    "zip",        // Creates a sequence of pairs from two sequences
    "contains",   // Returns a Boolean indicating whether the sequence contains an element
    "reversed",   // Returns a view presenting the elements in reverse order
    "enumerated", // Returns a sequence of pairs (n, x) where n is the index and x is the element
    "stride", // Returns a sequence from a starting value to, but not including, an end value, stepping by a specific amount

    // Mathematical Functions
    "abs",   // Returns the absolute value of a number
    "sqrt",  // Returns the square root of a number
    "pow",   // Returns the result of raising the first parameter to the power of the second
    "sin",   // Returns the sine of an angle measured in radians
    "cos",   // Returns the cosine of an angle measured in radians
    "tan",   // Returns the tangent of an angle measured in radians
    "round", // Returns the rounded value of a number
    "floor", // Returns the largest integer not greater than the given number
    "ceil",  // Returns the smallest integer not less than the given number

    // Memory Management Functions
    "withUnsafePointer",        // Invokes a closure with a pointer to a value
    "withUnsafeMutablePointer", // Invokes a closure with a mutable pointer to a value
    "withUnsafeBytes",          // Invokes a closure with the bytes of a value
    "withUnsafeMutableBytes",   // Invokes a closure with mutable bytes of a value
    "autoreleasepool",          // Creates an autorelease pool and executes the given closure within it

    // Runtime Functions
    "type",                   // Returns the dynamic type of a value
    "MemoryLayout.size",      // Returns the size in bytes of the given value's type
    "MemoryLayout.stride",    // Returns the stride in bytes of the given value's type
    "MemoryLayout.alignment", // Returns the alignment in bytes of the given value's type
    "unsafeBitCast",          // Reinterprets the bits of a value of one type as another type

    // Error Handling Functions
    "assert",              // Checks a condition and stops execution if it's false (in debug builds)
    "assertionFailure",    // Indicates that an assertion has failed (in debug builds)
    "precondition",        // Checks a condition and stops execution if it's false (in all builds)
    "preconditionFailure", // Indicates that a precondition has failed (in all builds)
    "fatalError",          // Unconditionally prints a message and stops execution

    // Miscellaneous Functions
    "swap",                // Exchanges the values of two variables
    "withExtendedLifetime" // Extends the lifetime of an object during the execution of a closure
  )

}
