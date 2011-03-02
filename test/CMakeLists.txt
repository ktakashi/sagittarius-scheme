INCLUDE_DIRECTORIES(${SAGITTARIUS_INCLUDE_DIR})

MACRO(add_c_test category testname)
  ADD_EXECUTABLE(${testname} c-modules/${category}/${testname}.c)
  TARGET_LINK_LIBRARIES(${testname} sagittarius)
  ADD_TEST(NAME c-${testname}
           COMMAND ${testname})
ENDMACRO(add_c_test)

ENABLE_TESTING()

# add_c_test(file file-test)
# add_c_test(port port-test)
# add_c_test(hash hashtable-test)
# add_c_test(reader reader-test)
# add_c_test(symbol symbol-test)
# add_c_test(bignum bignum-test)
# add_c_test(string string-test)
# add_c_test(write write-test)