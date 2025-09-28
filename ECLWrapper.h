#ifndef ECLWRAPPER_H
#define ECLWRAPPER_H

// This wrapper ensures __GMP_NO_CXX is defined before including ECL
extern "C" {
//#define __GMP_NO_CXX
#include <ecl/ecl.h>
}

#endif // ECLWRAPPER_H
