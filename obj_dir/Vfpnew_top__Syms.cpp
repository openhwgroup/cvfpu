// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Symbol table implementation internals

#include "Vfpnew_top__pch.h"
#include "Vfpnew_top.h"
#include "Vfpnew_top___024root.h"

// FUNCTIONS
Vfpnew_top__Syms::~Vfpnew_top__Syms()
{
}

Vfpnew_top__Syms::Vfpnew_top__Syms(VerilatedContext* contextp, const char* namep, Vfpnew_top* modelp)
    : VerilatedSyms{contextp}
    // Setup internal state of the Syms class
    , __Vm_modelp{modelp}
    // Setup module instances
    , TOP{this, namep}
{
        // Check resources
        Verilated::stackCheck(265);
    // Configure time unit / time precision
    _vm_contextp__->timeunit(-12);
    _vm_contextp__->timeprecision(-12);
    // Setup each module's pointers to their submodules
    // Setup each module's pointer back to symbol table (for public functions)
    TOP.__Vconfigure(true);
}
