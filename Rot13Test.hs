import Rot13

main :: IO ()
main = do
  putStrLn "Testing ROT13 implementation:"
  putStrLn ""

  -- Basic tests
  test "test" "grfg"
  test "Test" "Grfg"

  -- Full alphabet
  test "abcdefghijklmnopqrstuvwxyz" "nopqrstuvwxyzabcdefghijklm"
  test "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "NOPQRSTUVWXYZABCDEFGHIJKLM"

  -- Mixed case
  test "Hello World" "Uryyb Jbeyq"
  test "HeLLo WoRLd" "UrYYb JbEYq"

  -- Numbers and special characters
  test "123" "123"
  test "abc123xyz" "nop123klm"
  test "Hello123World!" "Uryyb123Jbeyq!"
  test "!@#$%^&*()" "!@#$%^&*()"

  -- Empty and single character
  test "" ""
  test "a" "n"
  test "A" "N"
  test "z" "m"
  test "Z" "M"
  test "n" "a"
  test "N" "A"

  -- Double ROT13 should give original
  testDouble "Hello World"
  testDouble "test123"
  testDouble "ABCXYZ"

  -- Edge cases
  test "aAbBzZ" "nNoOmM"
  test "The quick brown fox jumps over the lazy dog." "Gur dhvpx oebja sbk whzcf bire gur ynml qbt."

  -- Numbers mixed with letters
  test "a1b2c3" "n1o2p3"
  test "Test123Test" "Grfg123Grfg"

  -- Special characters and spaces
  test "Hello, World!" "Uryyb, Jbeyq!"
  test "ROT13 example." "EBG13 rknzcyr."
  test "Why did the chicken cross the road?" "Jul qvq gur puvpxra pebff gur ebnq?"

  -- Unicode/non-latin should stay unchanged
  test "café" "pnsé"  -- only latin letters shift

  putStrLn ""
  putStrLn "All tests completed!"

test :: String -> String -> IO ()
test input expected = do
  let result = rot13 input
  if result == expected
    then putStrLn $ "✓ rot13 \"" ++ input ++ "\" = \"" ++ result ++ "\""
    else putStrLn $ "✗ FAILED: rot13 \"" ++ input ++ "\" = \"" ++ result ++ "\" (expected \"" ++ expected ++ "\")"

testDouble :: String -> IO ()
testDouble input = do
  let result = rot13 (rot13 input)
  if result == input
    then putStrLn $ "✓ Double ROT13 \"" ++ input ++ "\" = \"" ++ result ++ "\""
    else putStrLn $ "✗ FAILED: Double ROT13 \"" ++ input ++ "\" = \"" ++ result ++ "\" (expected \"" ++ input ++ "\")"
