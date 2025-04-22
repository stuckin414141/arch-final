// This is a simple comment at the start of the file
let x: int := 5; // This is a comment after a statement

// Empty line comment

// Comment with special characters: !@#$%^&*()_+-={}[]:";'<>,.?/

// Comment with numbers: 123456789

// Multi-word comment spanning the entire line - the lexer should ignore all of this text

let y: int := 10; // Comment with variable names like x and y and keywords like if while let

// Consecutive
// Comment
// Lines

print(x + y); // Comment after expression

// Comment before control structure
if x > y then print(x) // Comment after then statement

// Test comment at the end of a block
while x > 0 {
    x := x - 1;
    print(x)
    // Comment inside block
}

// Comment before closing bracket
}; // This closing bracket should cause a parse error, not a lexing error

// Test comment with code-like content that shouldn't be executed:
// let z: int := 100; print(z);

// Test lexer's handling of language tokens in comments:
// := + - * / && || != == < > <= >= ( ) { } ; : if else while break

// Comment with nested comment-like sequence
// This line has // another comment marker that should be ignored

// Test escape-like sequences in comments: \n \t \\ \" \' 

// Final comment at the end of the file without newline