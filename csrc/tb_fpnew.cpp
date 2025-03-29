#include <stdlib.h>
#include <iostream>
#include <cstdlib>
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vfpnew_top.h"
// #include "Vfpnew_top___024unit.h"
#include "Vfpnew_top_fpnew_pkg.h"

#define MAX_SIM_TIME 300
#define VERIF_START_TIME 7
vluint64_t sim_time = 0;
vluint64_t posedge_cnt = 0;

void dut_reset(Vfpnew_top *dut, vluint64_t &sim_time) {
    dut->rst_ni = 0;
    if (sim_time >= 3 && sim_time < 6) {
        dut->rst_ni = 1;
        // 初始化所有输入信号
        dut->operands_i[0] = 0;
        dut->operands_i[1] = 0;
        dut->operands_i[2] = 0;
        dut->rnd_mode_i = Vfpnew_top_fpnew_pkg::RNE;  // 设置为向最近偶数舍入
        dut->op_i = Vfpnew_top_fpnew_pkg::MUL;        // 设置为乘法操作
        dut->op_mod_i = 0;
        dut->src_fmt_i = Vfpnew_top_fpnew_pkg::FP16;  // 源格式FP16
        dut->dst_fmt_i = Vfpnew_top_fpnew_pkg::FP16;  // 目标格式FP16
        dut->int_fmt_i = Vfpnew_top_fpnew_pkg::INT32; // 设置一个默认值
        dut->vectorial_op_i = 0;
        dut->tag_i = 0;
        dut->simd_mask_i = 1;
        dut->in_valid_i = 0;
        dut->flush_i = 0;
        dut->out_ready_i = 1;
    }
}

void set_random_input(Vfpnew_top *dut) {
    dut->operands_i[0] = rand() & 0xFFFF;
    dut->operands_i[1] = rand() & 0xFFFF;
    dut->in_valid_i = 1;
}

void check_output(Vfpnew_top *dut, vluint64_t &sim_time) {
    if (dut->out_valid_o) {
        std::cout << "Time: " << sim_time
                  << ", Result: 0x" << std::hex << dut->result_o
                  << ", Status: 0x" << std::hex << (int)dut->status_o
                  << std::dec << std::endl;
    }
}

int main(int argc, char** argv, char** env) {
    srand(time(NULL));
    Verilated::commandArgs(argc, argv);
    Vfpnew_top *dut = new Vfpnew_top;

    Verilated::traceEverOn(true);
    VerilatedVcdC *m_trace = new VerilatedVcdC;
    dut->trace(m_trace, 5);
    m_trace->open("waveform.vcd");

    while (sim_time < MAX_SIM_TIME) {
        dut_reset(dut, sim_time);

        dut->clk_i ^= 1;
        dut->eval();

        if (dut->clk_i == 1) {
            posedge_cnt++;
            if (sim_time >= VERIF_START_TIME) {
                set_random_input(dut);
                check_output(dut, sim_time);
            }
        }

        m_trace->dump(sim_time);
        sim_time++;
    }

    m_trace->close();
    delete dut;
    exit(EXIT_SUCCESS);
}