#include <stdlib.h>
#include <iostream>
#include <cstdlib>
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vfpnew_top.h"
#include "Vfpnew_top___024unit.h"

#define MAX_SIM_TIME 300
#define VERIF_START_TIME 7
vluint64_t sim_time = 0;
vluint64_t posedge_cnt = 0;

void dut_reset(Vfpnew_top *dut, vluint64_t &sim_time) {
    dut->rst_ni = 0;
    if (sim_time >= 3 && sim_time < 6) {
        dut->rst_ni = 1;
        // 初始化所有输入信号
        dut->operands_i = {0, 0, 0};
        dut->op_i = fpnew_pkg::MUL;       // 设置为乘法操作
        dut->src_fmt_i = fpnew_pkg::FP16;  // 源格式FP16
        dut->dst_fmt_i = fpnew_pkg::FP16;  // 目标格式FP16
        dut->in_valid_i = 0;
    }
}


void check_output(Vfpnew_top *dut, vluint64_t &sim_time) {
    static unsigned char in_valid_d = 0;
    static unsigned char out_valid_exp = 0;
    
    if (sim_time >= VERIF_START_TIME) {
        out_valid_exp = in_valid_d;
        in_valid_d = dut->in_valid_i;
        // 验证输出有效性
        if (dut->out_valid_o != out_valid_exp) {
            std::cout << "ERROR: out_valid mismatch at " << sim_time 
                      << " exp: " << (int)out_valid_exp
                      << " got: " << (int)dut->out_valid_o << std::endl;
        }
        // 当输出有效时打印结果
        if (dut->out_valid_o) {
            std::cout << "Result: 0x" << std::hex << dut->result_o 
                      << " Status: " << std::hex << dut->status_o
                      << std::dec << std::endl;
        }
    }
}


void set_rnd_out_valid(Valu *dut, vluint64_t &sim_time){
    if (sim_time >= VERIF_START_TIME) {
        dut->in_valid = rand() % 2;
    }
}

int main(int argc, char** argv, char** env) {
    srand (time(NULL));
    Verilated::commandArgs(argc, argv);
    Valu *dut = new Valu;

    Verilated::traceEverOn(true);
    VerilatedVcdC *m_trace = new VerilatedVcdC;
    dut->trace(m_trace, 5);
    m_trace->open("waveform.vcd");

    while (sim_time < MAX_SIM_TIME) {
        dut_reset(dut, sim_time);

        dut->clk ^= 1;
        dut->eval();

        if (dut->clk == 1){
            dut->in_valid = 0;
            posedge_cnt++;
            set_rnd_out_valid(dut, sim_time);
            check_out_valid(dut, sim_time);
        }

        m_trace->dump(sim_time);
        sim_time++;
    }

    m_trace->close();
    delete dut;
    exit(EXIT_SUCCESS);
}
