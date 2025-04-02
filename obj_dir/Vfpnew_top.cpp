// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Model implementation (design independent parts)

#include "Vfpnew_top__pch.h"
#include "verilated_vcd_c.h"

//============================================================
// Constructors

Vfpnew_top::Vfpnew_top(VerilatedContext* _vcontextp__, const char* _vcname__)
    : VerilatedModel{*_vcontextp__}
    , vlSymsp{new Vfpnew_top__Syms(contextp(), _vcname__, this)}
    , clk_i{vlSymsp->TOP.clk_i}
    , rst_ni{vlSymsp->TOP.rst_ni}
    , rnd_mode_i{vlSymsp->TOP.rnd_mode_i}
    , op_i{vlSymsp->TOP.op_i}
    , op_mod_i{vlSymsp->TOP.op_mod_i}
    , src_fmt_i{vlSymsp->TOP.src_fmt_i}
    , dst_fmt_i{vlSymsp->TOP.dst_fmt_i}
    , int_fmt_i{vlSymsp->TOP.int_fmt_i}
    , vectorial_op_i{vlSymsp->TOP.vectorial_op_i}
    , tag_i{vlSymsp->TOP.tag_i}
    , simd_mask_i{vlSymsp->TOP.simd_mask_i}
    , in_valid_i{vlSymsp->TOP.in_valid_i}
    , in_ready_o{vlSymsp->TOP.in_ready_o}
    , flush_i{vlSymsp->TOP.flush_i}
    , status_o{vlSymsp->TOP.status_o}
    , tag_o{vlSymsp->TOP.tag_o}
    , out_valid_o{vlSymsp->TOP.out_valid_o}
    , out_ready_i{vlSymsp->TOP.out_ready_i}
    , busy_o{vlSymsp->TOP.busy_o}
    , operands_i{vlSymsp->TOP.operands_i}
    , result_o{vlSymsp->TOP.result_o}
    , rootp{&(vlSymsp->TOP)}
{
    // Register model with the context
    contextp()->addModel(this);
    contextp()->traceBaseModelCbAdd(
        [this](VerilatedTraceBaseC* tfp, int levels, int options) { traceBaseModel(tfp, levels, options); });
}

Vfpnew_top::Vfpnew_top(const char* _vcname__)
    : Vfpnew_top(Verilated::threadContextp(), _vcname__)
{
}

//============================================================
// Destructor

Vfpnew_top::~Vfpnew_top() {
    delete vlSymsp;
}

//============================================================
// Evaluation function

#ifdef VL_DEBUG
void Vfpnew_top___024root___eval_debug_assertions(Vfpnew_top___024root* vlSelf);
#endif  // VL_DEBUG
void Vfpnew_top___024root___eval_static(Vfpnew_top___024root* vlSelf);
void Vfpnew_top___024root___eval_initial(Vfpnew_top___024root* vlSelf);
void Vfpnew_top___024root___eval_settle(Vfpnew_top___024root* vlSelf);
void Vfpnew_top___024root___eval(Vfpnew_top___024root* vlSelf);

void Vfpnew_top::eval_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+++++TOP Evaluate Vfpnew_top::eval_step\n"); );
#ifdef VL_DEBUG
    // Debug assertions
    Vfpnew_top___024root___eval_debug_assertions(&(vlSymsp->TOP));
#endif  // VL_DEBUG
    vlSymsp->__Vm_activity = true;
    vlSymsp->__Vm_deleter.deleteAll();
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) {
        vlSymsp->__Vm_didInit = true;
        VL_DEBUG_IF(VL_DBG_MSGF("+ Initial\n"););
        Vfpnew_top___024root___eval_static(&(vlSymsp->TOP));
        Vfpnew_top___024root___eval_initial(&(vlSymsp->TOP));
        Vfpnew_top___024root___eval_settle(&(vlSymsp->TOP));
    }
    VL_DEBUG_IF(VL_DBG_MSGF("+ Eval\n"););
    Vfpnew_top___024root___eval(&(vlSymsp->TOP));
    // Evaluate cleanup
    Verilated::endOfEval(vlSymsp->__Vm_evalMsgQp);
}

//============================================================
// Events and timing
bool Vfpnew_top::eventsPending() { return false; }

uint64_t Vfpnew_top::nextTimeSlot() {
    VL_FATAL_MT(__FILE__, __LINE__, "", "No delays in the design");
    return 0;
}

//============================================================
// Utilities

const char* Vfpnew_top::name() const {
    return vlSymsp->name();
}

//============================================================
// Invoke final blocks

void Vfpnew_top___024root___eval_final(Vfpnew_top___024root* vlSelf);

VL_ATTR_COLD void Vfpnew_top::final() {
    Vfpnew_top___024root___eval_final(&(vlSymsp->TOP));
}

//============================================================
// Implementations of abstract methods from VerilatedModel

const char* Vfpnew_top::hierName() const { return vlSymsp->name(); }
const char* Vfpnew_top::modelName() const { return "Vfpnew_top"; }
unsigned Vfpnew_top::threads() const { return 1; }
void Vfpnew_top::prepareClone() const { contextp()->prepareClone(); }
void Vfpnew_top::atClone() const {
    contextp()->threadPoolpOnClone();
}
std::unique_ptr<VerilatedTraceConfig> Vfpnew_top::traceConfig() const {
    return std::unique_ptr<VerilatedTraceConfig>{new VerilatedTraceConfig{false, false, false}};
};

//============================================================
// Trace configuration

void Vfpnew_top___024root__trace_decl_types(VerilatedVcd* tracep);

void Vfpnew_top___024root__trace_init_top(Vfpnew_top___024root* vlSelf, VerilatedVcd* tracep);

VL_ATTR_COLD static void trace_init(void* voidSelf, VerilatedVcd* tracep, uint32_t code) {
    // Callback from tracep->open()
    Vfpnew_top___024root* const __restrict vlSelf VL_ATTR_UNUSED = static_cast<Vfpnew_top___024root*>(voidSelf);
    Vfpnew_top__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    if (!vlSymsp->_vm_contextp__->calcUnusedSigs()) {
        VL_FATAL_MT(__FILE__, __LINE__, __FILE__,
            "Turning on wave traces requires Verilated::traceEverOn(true) call before time 0.");
    }
    vlSymsp->__Vm_baseCode = code;
    tracep->pushPrefix(std::string{vlSymsp->name()}, VerilatedTracePrefixType::SCOPE_MODULE);
    Vfpnew_top___024root__trace_decl_types(tracep);
    Vfpnew_top___024root__trace_init_top(vlSelf, tracep);
    tracep->popPrefix();
}

VL_ATTR_COLD void Vfpnew_top___024root__trace_register(Vfpnew_top___024root* vlSelf, VerilatedVcd* tracep);

VL_ATTR_COLD void Vfpnew_top::traceBaseModel(VerilatedTraceBaseC* tfp, int levels, int options) {
    (void)levels; (void)options;
    VerilatedVcdC* const stfp = dynamic_cast<VerilatedVcdC*>(tfp);
    if (VL_UNLIKELY(!stfp)) {
        vl_fatal(__FILE__, __LINE__, __FILE__,"'Vfpnew_top::trace()' called on non-VerilatedVcdC object;"
            " use --trace-fst with VerilatedFst object, and --trace with VerilatedVcd object");
    }
    stfp->spTrace()->addModel(this);
    stfp->spTrace()->addInitCb(&trace_init, &(vlSymsp->TOP));
    Vfpnew_top___024root__trace_register(&(vlSymsp->TOP), stfp->spTrace());
}
