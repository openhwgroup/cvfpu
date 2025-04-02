// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vfpnew_top.h for the primary calling header

#include "Vfpnew_top__pch.h"
#include "Vfpnew_top__Syms.h"
#include "Vfpnew_top___024root.h"

#ifdef VL_DEBUG
VL_ATTR_COLD void Vfpnew_top___024root___dump_triggers__stl(Vfpnew_top___024root* vlSelf);
#endif  // VL_DEBUG

VL_ATTR_COLD void Vfpnew_top___024root___eval_triggers__stl(Vfpnew_top___024root* vlSelf) {
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vfpnew_top___024root___eval_triggers__stl\n"); );
    Vfpnew_top__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    auto& vlSelfRef = std::ref(*vlSelf).get();
    // Body
    vlSelfRef.__VstlTriggered.setBit(0U, (IData)(vlSelfRef.__VstlFirstIteration));
    vlSelfRef.__VstlTriggered.setBit(1U, ((IData)(vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__inp_pipe_ready__BRA__1__KET__) 
                                          != (IData)(vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__inp_pipe_ready__BRA__1__KET____0)));
    vlSelfRef.__VstlTriggered.setBit(2U, ((IData)(vlSelfRef.fpnew_top__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes) 
                                          != (IData)(vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0)));
    vlSelfRef.__VstlTriggered.setBit(3U, (vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes 
                                          != vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0));
    vlSelfRef.__VstlTriggered.setBit(4U, ((IData)(vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__regular_status) 
                                          != (IData)(vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__regular_status__0)));
    vlSelfRef.__VstlTriggered.setBit(5U, (vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__1__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes 
                                          != vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__1__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0));
    vlSelfRef.__VstlTriggered.setBit(6U, (vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__2__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes 
                                          != vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__2__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0));
    vlSelfRef.__VstlTriggered.setBit(7U, (vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__3__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes 
                                          != vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__3__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0));
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__inp_pipe_ready__BRA__1__KET____0 
        = vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__inp_pipe_ready__BRA__1__KET__;
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0 
        = vlSelfRef.fpnew_top__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes;
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0 
        = vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes;
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__regular_status__0 
        = vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__0__KET____DOT__i_opgroup_block__DOT__gen_parallel_slices__BRA__2__KET____DOT__active_format__DOT__i_fmt_slice__DOT__gen_num_lanes__BRA__0__KET____DOT__active_lane__DOT__lane_instance__DOT__i_fma__DOT__regular_status;
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__1__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0 
        = vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__1__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes;
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__2__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0 
        = vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__2__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes;
    vlSelfRef.__Vtrigprevexpr___TOP__fpnew_top__DOT__gen_operation_groups__BRA__3__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes__0 
        = vlSelfRef.fpnew_top__DOT__gen_operation_groups__BRA__3__KET____DOT__i_opgroup_block__DOT__i_arbiter__DOT__gen_arbiter__DOT__index_nodes;
    if (VL_UNLIKELY(((1U & (~ (IData)(vlSelfRef.__VstlDidInit)))))) {
        vlSelfRef.__VstlDidInit = 1U;
        vlSelfRef.__VstlTriggered.setBit(1U, 1U);
        vlSelfRef.__VstlTriggered.setBit(2U, 1U);
        vlSelfRef.__VstlTriggered.setBit(3U, 1U);
        vlSelfRef.__VstlTriggered.setBit(4U, 1U);
        vlSelfRef.__VstlTriggered.setBit(5U, 1U);
        vlSelfRef.__VstlTriggered.setBit(6U, 1U);
        vlSelfRef.__VstlTriggered.setBit(7U, 1U);
    }
#ifdef VL_DEBUG
    if (VL_UNLIKELY(vlSymsp->_vm_contextp__->debug())) {
        Vfpnew_top___024root___dump_triggers__stl(vlSelf);
    }
#endif
}
