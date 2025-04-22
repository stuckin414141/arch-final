// Test operator precedence
print(1 + 2 * 3);          // Should be 7, not 9
print((1 + 2) * 3);        // Should be 9
print(10 - 5 - 2);         // Should be 3, not 7
print(10 - (5 - 2));       // Should be 7

// Test logical operator precedence
print(true && false || true);  // Should be true
print(true && (false || true)); // Should be true
print((true && false) || true); // Should be true

// Test bitwise operators precedence
print(5 & 3 | 4);          // Test precedence
print(5 & (3 | 4));        // Different result