#include <stdlib.h>
#include <iostream>
#include <cstdlib>
#include <iomanip>  
#include <cmath> 
#include <queue>  
#include <verilated.h>
#include <verilated_vcd_c.h>
#include "Vfpnew_top.h"
// #include "Vfpnew_top___024unit.h"
// #include "Vfpnew_top_fpnew_pkg.h"

#define MAX_SIM_TIME 100
#define VERIF_START_TIME 7
vluint64_t sim_time = 0;
vluint64_t posedge_cnt = 0;

void dut_reset(Vfpnew_top *dut, vluint64_t &sim_time) {
    dut->rst_ni = 1;
    if (sim_time >= 3 && sim_time < 6) {
        dut->rst_ni = 0;
        // 初始化所有输入信号
        dut->operands_i = 0;
        dut->rnd_mode_i = 0;  // 设置为向最近偶数舍入
        dut->op_i = 3;        // 设置为乘法操作
        dut->op_mod_i = 0;
        dut->src_fmt_i = 2;  // 源格式FP16
        dut->dst_fmt_i = 2;  // 目标格式FP16
        dut->int_fmt_i = 2; // 设置一个默认值
        dut->vectorial_op_i = 0;
        dut->tag_i = 0;
        dut->simd_mask_i = 1;
        dut->in_valid_i = 0;
        dut->flush_i = 0;
        dut->out_ready_i = 1;
    }
}

struct InputPair {
    uint16_t a;
    uint16_t b;
};

uint16_t fp16_multiply(uint16_t a, uint16_t b) {
    // 提取符号位
    int sign_a = (a >> 15) & 1;
    int sign_b = (b >> 15) & 1;
    int sign_result = sign_a ^ sign_b;

    // 提取指数
    int exp_a = (a >> 10) & 0x1F;
    int exp_b = (b >> 10) & 0x1F;

    // 提取尾数
    int frac_a = a & 0x3FF;
    int frac_b = b & 0x3FF;

    // 特殊情况处理：0, inf, NaN
    if (exp_a == 0 && frac_a == 0) return 0;
    if (exp_b == 0 && frac_b == 0) return 0;
    if (exp_a == 31 || exp_b == 31) return 0x7C00 | (sign_result << 15); // Inf or NaN

    // 正常数字处理
    exp_a = (exp_a == 0) ? 1 : exp_a;
    exp_b = (exp_b == 0) ? 1 : exp_b;
    frac_a = (exp_a == 0) ? frac_a : (frac_a | 0x400);
    frac_b = (exp_b == 0) ? frac_b : (frac_b | 0x400);

    // 计算结果
    int exp_result = exp_a + exp_b - 15;
    int64_t frac_result = (int64_t)frac_a * frac_b;

    // 规范化结果
    while (frac_result > 0x7FFFFF) {
        frac_result >>= 1;
        exp_result++;
    }
    while (frac_result <= 0x3FFFFF) {
        frac_result <<= 1;
        exp_result--;
    }

    // 舍入（这里使用向零舍入，可以根据需要修改）
    frac_result = (frac_result >> 13) & 0x3FF;

    // 处理上溢和下溢
    if (exp_result >= 31) return 0x7C00 | (sign_result << 15); // Inf
    if (exp_result <= 0) return 0; // 下溢到0

    // 组装结果
    return (sign_result << 15) | (exp_result << 10) | frac_result;
}

std::queue<InputPair> input_queue;
uint16_t generate_random_fp16() {
    uint16_t sign = rand() & 0x1;
    uint16_t exp = rand() & 0x1F;
    uint16_t frac = rand() & 0x3FF;
    return (sign << 15) | (exp << 10) | frac;
}

// 新增：将半精度浮点数转换为浮点数
float fp16_to_float(uint16_t fp16) {
    int sign = (fp16 >> 15) & 0x1;
    int exp = (fp16 >> 10) & 0x1F;
    int frac = fp16 & 0x3FF;
    
    if (exp == 0 && frac == 0) return sign ? -0.0f : 0.0f;
    if (exp == 0x1F) return frac == 0 ? (sign ? -INFINITY : INFINITY) : NAN;
    
    float result = (1.0f + (float)frac / 1024.0f) * std::pow(2.0f, exp - 15);
    return sign ? -result : result;
}


void set_random_input(Vfpnew_top *dut) {
    uint16_t fp16_a = generate_random_fp16();
    uint16_t fp16_b = generate_random_fp16();
    dut->operands_i = (uint32_t)fp16_a << 16 | fp16_b;
    dut->in_valid_i = 1;
    
    InputPair pair = {fp16_a, fp16_b};
    input_queue.push(pair);

    std::cout << "Input at time " << sim_time << ": "
    << "A = 0x" << std::hex << fp16_a << ", "
    << "B = 0x" << fp16_b << std::dec << std::endl;
}

void check_output(Vfpnew_top *dut, vluint64_t &sim_time) {
    if (dut->out_valid_o && !input_queue.empty()) {
        InputPair input = input_queue.front();
        input_queue.pop();
        
        float a = fp16_to_float(input.a);
        float b = fp16_to_float(input.b);
        float hw_result = fp16_to_float(dut->result_o);
        uint16_t sw_result_fp16 = fp16_multiply(input.a, input.b);
        float sw_result = fp16_to_float(sw_result_fp16);
        float diff = std::abs(hw_result - sw_result);

        std::cout << "Output at time " << sim_time << std::endl;
        std::cout << "Input A: " << std::setprecision(6) << a 
                  << " (0x" << std::hex << input.a << std::dec << ")" << std::endl;
        std::cout << "Input B: " << std::setprecision(6) << b 
                  << " (0x" << std::hex << input.b << std::dec << ")" << std::endl;
        std::cout << "HW Result: " << std::setprecision(6) << hw_result 
                  << " (0x" << std::hex << dut->result_o << std::dec << ")" << std::endl;
        std::cout << "SW Result: " << std::setprecision(6) << sw_result 
                  << " (0x" << std::hex << sw_result_fp16 << std::dec << ")" << std::endl;
        std::cout << "Difference: " << std::setprecision(6) << diff << std::endl;
        std::cout << "Within tolerance: " << (diff < 1e-2 ? "Yes" : "No") << std::endl;
        std::cout << "Status: 0x" << std::hex << (int)dut->status_o << std::dec << std::endl;
        std::cout << "------------------------" << std::endl;
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
            }
            check_output(dut, sim_time);
        }

        m_trace->dump(sim_time);
        sim_time++;
    }

    m_trace->close();
    delete dut;
    exit(EXIT_SUCCESS);
}