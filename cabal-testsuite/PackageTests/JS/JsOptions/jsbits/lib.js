//#OPTIONS: CPP

function foo() {
#ifdef PRINT_DEF
  console.log("Hello definition!");
#else
  console.log("Hello!");
#endif
}
