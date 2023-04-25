/*Copyright 2020-2021 T-Head Semiconductor Co., Ltd.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

// &ModuleBeg; @24
module pa_fpu_dp(
  cp0_fpu_icg_en,
  cp0_fpu_xx_rm,
  cp0_yy_clk_en,
  ctrl_xx_ex1_inst_vld,
  ctrl_xx_ex1_stall,
  ctrl_xx_ex1_warm_up,
  ctrl_xx_ex2_inst_vld,
  ctrl_xx_ex2_stall,
  ctrl_xx_ex2_warm_up,
  ctrl_xx_ex3_inst_vld,
  ctrl_xx_ex3_stall,
  ctrl_xx_ex3_warm_up,
  dp_frbus_ex2_data,
  dp_frbus_ex2_fflags,
  dp_frbus_ex2_freg,
  dp_frbus_ex3_data,
  dp_frbus_ex3_fflags,
  dp_frbus_ex3_freg,
  dp_frbus_ex4_data,
  dp_frbus_ex4_fflags,
  dp_frbus_ex4_freg,
  dp_xx_ex1_cnan,
  dp_xx_ex1_id,
  dp_xx_ex1_inf,
  dp_xx_ex1_norm,
  dp_xx_ex1_qnan,
  dp_xx_ex1_rm,
  dp_xx_ex1_snan,
  dp_xx_ex1_srcf2,
  dp_xx_ex1_zero,
  dp_xx_ex2_rm,
  dp_xx_ex2_srcf2,
  dp_xx_ex3_rm,
  dp_xx_ex4_rm,
  ex2_inst_wb,
  falu_fpu_ex1_fflags,
  falu_fpu_ex1_result,
  falu_fpu_ex1_special_sel,
  falu_fpu_ex1_special_sign,
  falu_fpu_ex1_src_reuse,
  falu_fpu_ex1_src_reuse_data,
  falu_fpu_ex1_wb_gpr,
  falu_fpu_ex2_fflags,
  falu_fpu_ex2_result,
  falu_fpu_ex2_result_vld,
  falu_fpu_ex3_fflags,
  falu_fpu_ex3_result,
  falu_fpu_ex3_result_vld,
  falu_fpu_ex3_wb_gpr,
  fdsu_fpu_ex1_fflags,
  fdsu_fpu_ex1_special_sel,
  fdsu_fpu_ex1_special_sign,
  fmau_fpu_ex1_fflags,
  fmau_fpu_ex1_special_sel,
  fmau_fpu_ex1_special_sign,
  fmau_fpu_ex3_fflags,
  fmau_fpu_ex3_result,
  fmau_fpu_ex3_result_vld,
  fmau_fpu_ex4_fflags,
  fmau_fpu_ex4_result,
  forever_cpuclk,
  idu_fpu_ex1_dst_freg,
  idu_fpu_ex1_dst_freg_vld,
  idu_fpu_ex1_eu_sel,
  idu_fpu_ex1_func,
  idu_fpu_ex1_gateclk_vld,
  idu_fpu_ex1_rm,
  idu_fpu_ex1_srcf0,
  idu_fpu_ex1_srcf1,
  idu_fpu_ex1_srcf2,
  pad_yy_icg_scan_en
);

// &Ports; @25
input           cp0_fpu_icg_en;             
input   [2 :0]  cp0_fpu_xx_rm;              
input           cp0_yy_clk_en;              
input           ctrl_xx_ex1_inst_vld;       
input           ctrl_xx_ex1_stall;          
input           ctrl_xx_ex1_warm_up;        
input           ctrl_xx_ex2_inst_vld;       
input           ctrl_xx_ex2_stall;          
input           ctrl_xx_ex2_warm_up;        
input           ctrl_xx_ex3_inst_vld;       
input           ctrl_xx_ex3_stall;          
input           ctrl_xx_ex3_warm_up;        
input   [4 :0]  falu_fpu_ex1_fflags;        
input   [31:0]  falu_fpu_ex1_result;        
input   [8 :0]  falu_fpu_ex1_special_sel;   
input   [3 :0]  falu_fpu_ex1_special_sign;  
input           falu_fpu_ex1_src_reuse;     
input   [31:0]  falu_fpu_ex1_src_reuse_data; 
input           falu_fpu_ex1_wb_gpr;        
input   [4 :0]  falu_fpu_ex2_fflags;        
input   [31:0]  falu_fpu_ex2_result;        
input           falu_fpu_ex2_result_vld;    
input   [4 :0]  falu_fpu_ex3_fflags;        
input   [31:0]  falu_fpu_ex3_result;        
input           falu_fpu_ex3_result_vld;    
input           falu_fpu_ex3_wb_gpr;        
input   [4 :0]  fdsu_fpu_ex1_fflags;        
input   [7 :0]  fdsu_fpu_ex1_special_sel;   
input   [3 :0]  fdsu_fpu_ex1_special_sign;  
input   [4 :0]  fmau_fpu_ex1_fflags;        
input   [7 :0]  fmau_fpu_ex1_special_sel;   
input   [3 :0]  fmau_fpu_ex1_special_sign;  
input   [4 :0]  fmau_fpu_ex3_fflags;        
input   [31:0]  fmau_fpu_ex3_result;        
input           fmau_fpu_ex3_result_vld;    
input   [4 :0]  fmau_fpu_ex4_fflags;        
input   [31:0]  fmau_fpu_ex4_result;        
input           forever_cpuclk;             
input   [4 :0]  idu_fpu_ex1_dst_freg;       
input           idu_fpu_ex1_dst_freg_vld;   
input   [2 :0]  idu_fpu_ex1_eu_sel;         
input   [9 :0]  idu_fpu_ex1_func;           
input           idu_fpu_ex1_gateclk_vld;    
input   [2 :0]  idu_fpu_ex1_rm;             
input   [31:0]  idu_fpu_ex1_srcf0;          
input   [31:0]  idu_fpu_ex1_srcf1;          
input   [31:0]  idu_fpu_ex1_srcf2;          
input           pad_yy_icg_scan_en;         
output  [31:0]  dp_frbus_ex2_data;          
output  [4 :0]  dp_frbus_ex2_fflags;        
output  [4 :0]  dp_frbus_ex2_freg;          
output  [31:0]  dp_frbus_ex3_data;          
output  [4 :0]  dp_frbus_ex3_fflags;        
output  [4 :0]  dp_frbus_ex3_freg;          
output  [31:0]  dp_frbus_ex4_data;          
output  [4 :0]  dp_frbus_ex4_fflags;        
output  [4 :0]  dp_frbus_ex4_freg;          
output  [2 :0]  dp_xx_ex1_cnan;             
output  [2 :0]  dp_xx_ex1_id;               
output  [2 :0]  dp_xx_ex1_inf;              
output  [2 :0]  dp_xx_ex1_norm;             
output  [2 :0]  dp_xx_ex1_qnan;             
output  [2 :0]  dp_xx_ex1_rm;               
output  [2 :0]  dp_xx_ex1_snan;             
output  [31:0]  dp_xx_ex1_srcf2;            
output  [2 :0]  dp_xx_ex1_zero;             
output  [2 :0]  dp_xx_ex2_rm;               
output  [31:0]  dp_xx_ex2_srcf2;            
output  [2 :0]  dp_xx_ex3_rm;               
output  [2 :0]  dp_xx_ex4_rm;               
output          ex2_inst_wb;                

// &Regs; @26
reg     [4 :0]  ex1_fflags;                 
reg     [31:0]  ex1_special_data;           
reg     [8 :0]  ex1_special_sel;            
reg     [3 :0]  ex1_special_sign;           
reg     [4 :0]  ex2_fflags;                 
reg     [4 :0]  ex2_freg;                   
reg             ex2_freg_vld;               
reg     [31:0]  ex2_result;                 
reg     [2 :0]  ex2_rm;                     
reg     [31:0]  ex2_special_data;           
reg     [6 :0]  ex2_special_sel;            
reg     [3 :0]  ex2_special_sign;           
reg     [4 :0]  ex3_freg;                   
reg     [2 :0]  ex3_rm;                     
reg     [4 :0]  ex4_freg;                   
reg     [2 :0]  ex4_rm;                     

// &Wires; @27
wire            cp0_fpu_icg_en;             
wire    [2 :0]  cp0_fpu_xx_rm;              
wire            cp0_yy_clk_en;              
wire            ctrl_xx_ex1_inst_vld;       
wire            ctrl_xx_ex1_stall;          
wire            ctrl_xx_ex1_warm_up;        
wire            ctrl_xx_ex2_inst_vld;       
wire            ctrl_xx_ex2_stall;          
wire            ctrl_xx_ex2_warm_up;        
wire            ctrl_xx_ex3_inst_vld;       
wire            ctrl_xx_ex3_stall;          
wire            ctrl_xx_ex3_warm_up;        
wire    [31:0]  dp_frbus_ex2_data;          
wire    [4 :0]  dp_frbus_ex2_fflags;        
wire    [4 :0]  dp_frbus_ex2_freg;          
wire    [31:0]  dp_frbus_ex3_data;          
wire    [4 :0]  dp_frbus_ex3_fflags;        
wire    [4 :0]  dp_frbus_ex3_freg;          
wire    [31:0]  dp_frbus_ex4_data;          
wire    [4 :0]  dp_frbus_ex4_fflags;        
wire    [4 :0]  dp_frbus_ex4_freg;          
wire    [2 :0]  dp_xx_ex1_cnan;             
wire    [2 :0]  dp_xx_ex1_id;               
wire    [2 :0]  dp_xx_ex1_inf;              
wire    [2 :0]  dp_xx_ex1_norm;             
wire    [2 :0]  dp_xx_ex1_qnan;             
wire    [2 :0]  dp_xx_ex1_rm;               
wire    [2 :0]  dp_xx_ex1_snan;             
wire    [31:0]  dp_xx_ex1_srcf2;            
wire    [2 :0]  dp_xx_ex1_zero;             
wire    [2 :0]  dp_xx_ex2_rm;               
wire    [31:0]  dp_xx_ex2_srcf2;            
wire    [2 :0]  dp_xx_ex3_rm;               
wire    [2 :0]  dp_xx_ex4_rm;               
wire    [2 :0]  ex1_decode_rm;              
wire            ex1_double;                 
wire    [2 :0]  ex1_eu_sel;                 
wire    [4 :0]  ex1_freg;                   
wire            ex1_freg_vld;               
wire    [9 :0]  ex1_func;                   
wire    [2 :0]  ex1_global_rm;              
wire    [2 :0]  ex1_rm;                     
wire            ex1_single;                 
wire    [31:0]  ex1_special_data_final;     
wire    [63:0]  ex1_src0;                   
wire    [63:0]  ex1_src1;                   
wire    [63:0]  ex1_src2;                   
wire            ex1_src2_vld;               
wire    [2 :0]  ex1_src_cnan;               
wire    [2 :0]  ex1_src_id;                 
wire    [2 :0]  ex1_src_inf;                
wire    [2 :0]  ex1_src_norm;               
wire    [2 :0]  ex1_src_qnan;               
wire    [2 :0]  ex1_src_snan;               
wire    [2 :0]  ex1_src_zero;               
wire            ex2_data_clk;               
wire            ex2_data_clk_en;            
wire            ex2_inst_wb;                
wire            ex3_data_clk;               
wire            ex3_data_clk_en;            
wire            ex3_inst_wb;                
wire            ex4_data_clk;               
wire            ex4_data_clk_en;            
wire    [4 :0]  falu_fpu_ex1_fflags;        
wire    [31:0]  falu_fpu_ex1_result;        
wire    [8 :0]  falu_fpu_ex1_special_sel;   
wire    [3 :0]  falu_fpu_ex1_special_sign;  
wire            falu_fpu_ex1_src_reuse;     
wire    [31:0]  falu_fpu_ex1_src_reuse_data; 
wire            falu_fpu_ex1_wb_gpr;        
wire    [4 :0]  falu_fpu_ex2_fflags;        
wire    [31:0]  falu_fpu_ex2_result;        
wire            falu_fpu_ex2_result_vld;    
wire    [4 :0]  falu_fpu_ex3_fflags;        
wire    [31:0]  falu_fpu_ex3_result;        
wire            falu_fpu_ex3_result_vld;    
wire            falu_fpu_ex3_wb_gpr;        
wire    [4 :0]  fdsu_fpu_ex1_fflags;        
wire    [7 :0]  fdsu_fpu_ex1_special_sel;   
wire    [3 :0]  fdsu_fpu_ex1_special_sign;  
wire    [4 :0]  fmau_fpu_ex1_fflags;        
wire    [7 :0]  fmau_fpu_ex1_special_sel;   
wire    [3 :0]  fmau_fpu_ex1_special_sign;  
wire    [4 :0]  fmau_fpu_ex3_fflags;        
wire    [31:0]  fmau_fpu_ex3_result;        
wire            fmau_fpu_ex3_result_vld;    
wire    [4 :0]  fmau_fpu_ex4_fflags;        
wire    [31:0]  fmau_fpu_ex4_result;        
wire            forever_cpuclk;             
wire    [4 :0]  idu_fpu_ex1_dst_freg;       
wire            idu_fpu_ex1_dst_freg_vld;   
wire    [2 :0]  idu_fpu_ex1_eu_sel;         
wire    [9 :0]  idu_fpu_ex1_func;           
wire            idu_fpu_ex1_gateclk_vld;    
wire    [2 :0]  idu_fpu_ex1_rm;             
wire    [31:0]  idu_fpu_ex1_srcf0;          
wire    [31:0]  idu_fpu_ex1_srcf1;          
wire    [31:0]  idu_fpu_ex1_srcf2;          
wire            pad_yy_icg_scan_en;         


// &Depend("cpu_cfig.h"); @29
parameter DOUBLE_WIDTH =64;
parameter SINGLE_WIDTH =32;
parameter FUNC_WIDTH   =10;
//==========================================================
//                     EX1 special data path
//==========================================================
assign ex1_eu_sel[2:0]            = idu_fpu_ex1_eu_sel[2:0];
assign ex1_func[FUNC_WIDTH-1:0]   = idu_fpu_ex1_func[FUNC_WIDTH-1:0];
assign ex1_freg[4:0]              = idu_fpu_ex1_dst_freg[4:0];
assign ex1_freg_vld               = idu_fpu_ex1_dst_freg_vld;
assign ex1_global_rm[2:0]         = cp0_fpu_xx_rm[2:0];
assign ex1_decode_rm[2:0]         = idu_fpu_ex1_rm[2:0];

assign ex1_rm[2:0]                = (ex1_decode_rm[2:0]==3'b111) 
                                  ?  ex1_global_rm[2:0] : ex1_decode_rm[2:0]; 

assign ex1_src2_vld               = idu_fpu_ex1_eu_sel[1] && ex1_func[0];

assign dp_xx_ex1_rm[2:0]          = ex1_rm[2:0];
assign dp_xx_ex2_rm[2:0]          = ex2_rm[2:0];
assign dp_xx_ex3_rm[2:0]          = ex3_rm[2:0];
assign dp_xx_ex4_rm[2:0]          = ex4_rm[2:0];

assign ex1_src0[DOUBLE_WIDTH-1:0] = { {SINGLE_WIDTH{1'b1}},idu_fpu_ex1_srcf0[SINGLE_WIDTH-1:0]};
assign ex1_src1[DOUBLE_WIDTH-1:0] = { {SINGLE_WIDTH{1'b1}},idu_fpu_ex1_srcf1[SINGLE_WIDTH-1:0]};
assign ex1_src2[DOUBLE_WIDTH-1:0] = ex1_src2_vld ? { {SINGLE_WIDTH{1'b1}},idu_fpu_ex1_srcf2[SINGLE_WIDTH-1:0]}
                                                 : { {SINGLE_WIDTH{1'b1}},{SINGLE_WIDTH{1'b0}} };

assign ex1_double = 1'b0;
assign ex1_single = 1'b1;

//==========================================================
//                EX1 special src data judge
//==========================================================
// &Instance("pa_fpu_src_type","x_pa_fpu_ex1_srcf0_type"); @74
pa_fpu_src_type  x_pa_fpu_ex1_srcf0_type (
  .inst_double     (ex1_double     ),
  .inst_single     (ex1_single     ),
  .src_cnan        (ex1_src_cnan[0]),
  .src_id          (ex1_src_id[0]  ),
  .src_in          (ex1_src0       ),
  .src_inf         (ex1_src_inf[0] ),
  .src_norm        (ex1_src_norm[0]),
  .src_qnan        (ex1_src_qnan[0]),
  .src_snan        (ex1_src_snan[0]),
  .src_zero        (ex1_src_zero[0])
);

// &Connect(.src_in     (ex1_src0       ), @75
//          .inst_double(ex1_double     ), @76
//          .inst_single(ex1_single     ), @77
//          .src_cnan   (ex1_src_cnan[0]), @78
//          .src_snan   (ex1_src_snan[0]), @79
//          .src_qnan   (ex1_src_qnan[0]), @80
//          .src_norm   (ex1_src_norm[0]), @81
//          .src_zero   (ex1_src_zero[0]), @82
//          .src_inf    (ex1_src_inf[0] ), @83
//          .src_id     (ex1_src_id[0]  )); @84

// &Instance("pa_fpu_src_type","x_pa_fpu_ex1_srcf1_type"); @86
pa_fpu_src_type  x_pa_fpu_ex1_srcf1_type (
  .inst_double     (ex1_double     ),
  .inst_single     (ex1_single     ),
  .src_cnan        (ex1_src_cnan[1]),
  .src_id          (ex1_src_id[1]  ),
  .src_in          (ex1_src1       ),
  .src_inf         (ex1_src_inf[1] ),
  .src_norm        (ex1_src_norm[1]),
  .src_qnan        (ex1_src_qnan[1]),
  .src_snan        (ex1_src_snan[1]),
  .src_zero        (ex1_src_zero[1])
);

// &Connect(.src_in     (ex1_src1       ), @87
//          .inst_double(ex1_double     ), @88
//          .inst_single(ex1_single     ), @89
//          .src_cnan   (ex1_src_cnan[1]), @90
//          .src_snan   (ex1_src_snan[1]), @91
//          .src_qnan   (ex1_src_qnan[1]), @92
//          .src_norm   (ex1_src_norm[1]), @93
//          .src_zero   (ex1_src_zero[1]), @94
//          .src_inf    (ex1_src_inf[1] ), @95
//          .src_id     (ex1_src_id[1]  )); @96

// &Instance("pa_fpu_src_type","x_pa_fpu_ex1_srcf2_type"); @98
pa_fpu_src_type  x_pa_fpu_ex1_srcf2_type (
  .inst_double     (ex1_double     ),
  .inst_single     (ex1_single     ),
  .src_cnan        (ex1_src_cnan[2]),
  .src_id          (ex1_src_id[2]  ),
  .src_in          (ex1_src2       ),
  .src_inf         (ex1_src_inf[2] ),
  .src_norm        (ex1_src_norm[2]),
  .src_qnan        (ex1_src_qnan[2]),
  .src_snan        (ex1_src_snan[2]),
  .src_zero        (ex1_src_zero[2])
);

// &Connect(.src_in     (ex1_src2       ), @99
//          .inst_double(ex1_double     ), @100
//          .inst_single(ex1_single     ), @101
//          .src_cnan   (ex1_src_cnan[2]), @102
//          .src_snan   (ex1_src_snan[2]), @103
//          .src_qnan   (ex1_src_qnan[2]), @104
//          .src_norm   (ex1_src_norm[2]), @105
//          .src_zero   (ex1_src_zero[2]), @106
//          .src_inf    (ex1_src_inf[2] ), @107
//          .src_id     (ex1_src_id[2]  )); @108

assign dp_xx_ex1_cnan[2:0] = ex1_src_cnan[2:0];
assign dp_xx_ex1_snan[2:0] = ex1_src_snan[2:0];
assign dp_xx_ex1_qnan[2:0] = ex1_src_qnan[2:0];
assign dp_xx_ex1_norm[2:0] = ex1_src_norm[2:0];
assign dp_xx_ex1_zero[2:0] = ex1_src_zero[2:0];
assign dp_xx_ex1_inf[2:0]  = ex1_src_inf[2:0];
assign dp_xx_ex1_id[2:0]   = ex1_src_id[2:0];

//==========================================================
//                EX1 special result judge
//==========================================================

// &CombBeg; @122
always @( fmau_fpu_ex1_fflags[4:0]
       or fdsu_fpu_ex1_special_sign[3:0]
       or falu_fpu_ex1_fflags[4:0]
       or fdsu_fpu_ex1_fflags[4:0]
       or fmau_fpu_ex1_special_sign[3:0]
       or fmau_fpu_ex1_special_sel[7:0]
       or ex1_eu_sel[2:0]
       or fdsu_fpu_ex1_special_sel[7:0]
       or falu_fpu_ex1_special_sel[8:0]
       or falu_fpu_ex1_special_sign[3:0])
begin
case(ex1_eu_sel[2:0])
  3'b001: begin//FALU
         ex1_fflags[4:0]       = falu_fpu_ex1_fflags[4:0];
         ex1_special_sel[8:0]  = falu_fpu_ex1_special_sel[8:0]; //falu,qnan_src2,qnan_src1,qnan_src0,cnan,lfn,inf,zero,src2
         ex1_special_sign[3:0] = falu_fpu_ex1_special_sign[3:0]; //lfn,inf,zero,src2
         end
   3'b010: begin//FMAU
         ex1_fflags[4:0]       = fmau_fpu_ex1_fflags[4:0];
         ex1_special_sel[8:0]  ={1'b0,fmau_fpu_ex1_special_sel[7:0]};
         ex1_special_sign[3:0] = fmau_fpu_ex1_special_sign[3:0];
         end
   3'b100: begin//FDSU
         ex1_fflags[4:0]       = fdsu_fpu_ex1_fflags[4:0];
         ex1_special_sel[8:0]  ={1'b0,fdsu_fpu_ex1_special_sel[7:0]};
         ex1_special_sign[3:0] = fdsu_fpu_ex1_special_sign[3:0];
         end
default: begin//FDSU
         ex1_fflags[4:0]       = {5{1'b0}};
         ex1_special_sel[8:0]  = {9{1'b0}};
         ex1_special_sign[3:0] = {4{1'b0}};
         end
endcase
// &CombEnd; @145
end

// &CombBeg; @148
// &CombEnd; @156
// &CombBeg; @162
always @( falu_fpu_ex1_result[31:0]
       or ex1_special_sel[8:5]
       or ex1_src0[31:0]
       or ex1_src2[31:0]
       or ex1_src1[31:0])
begin
case(ex1_special_sel[8:5])
  4'b0001: ex1_special_data[SINGLE_WIDTH-1:0] = ex1_src0[SINGLE_WIDTH-1:0];
  4'b0010: ex1_special_data[SINGLE_WIDTH-1:0] = ex1_src1[SINGLE_WIDTH-1:0];
  4'b0100: ex1_special_data[SINGLE_WIDTH-1:0] = ex1_src2[SINGLE_WIDTH-1:0];
  4'b1000: ex1_special_data[SINGLE_WIDTH-1:0] = falu_fpu_ex1_result[SINGLE_WIDTH-1:0];
default  : ex1_special_data[SINGLE_WIDTH-1:0] = ex1_src2[SINGLE_WIDTH-1:0];
endcase
// &CombEnd; @170
end

assign ex1_special_data_final[SINGLE_WIDTH-1:0] = falu_fpu_ex1_src_reuse 
                                                ? falu_fpu_ex1_src_reuse_data[SINGLE_WIDTH-1:0]
                                                : ex1_special_data[SINGLE_WIDTH-1:0];
//==========================================================
//                     EX1-EX2 data pipedown
//==========================================================
assign ex2_data_clk_en = idu_fpu_ex1_gateclk_vld || ctrl_xx_ex1_warm_up;

// &Instance("gated_clk_cell", "x_fpu_data_ex2_gated_clk"); @181
gated_clk_cell  x_fpu_data_ex2_gated_clk (
  .clk_in             (forever_cpuclk    ),
  .clk_out            (ex2_data_clk      ),
  .external_en        (1'b0              ),
  .global_en          (cp0_yy_clk_en     ),
  .local_en           (ex2_data_clk_en   ),
  .module_en          (cp0_fpu_icg_en    ),
  .pad_yy_icg_scan_en (pad_yy_icg_scan_en)
);

// &Connect(.clk_in      (forever_cpuclk      ), @182
//          .external_en (1'b0                ), @183
//          .global_en   (cp0_yy_clk_en       ), @184
//          .module_en   (cp0_fpu_icg_en      ), @185
//          .local_en    (ex2_data_clk_en     ), @186
//          .clk_out     (ex2_data_clk        )); @187

always @(posedge ex2_data_clk)
begin
  if(ctrl_xx_ex1_inst_vld && !ctrl_xx_ex1_stall && !falu_fpu_ex1_wb_gpr || ctrl_xx_ex1_warm_up)
  begin
    ex2_rm[2:0]           <= ex1_rm[2:0];
    ex2_freg_vld          <= ex1_freg_vld;
    ex2_freg[4:0]         <= ex1_freg[4:0];
    ex2_fflags[4:0]       <= ex1_fflags[4:0];
    ex2_special_sign[3:0] <= ex1_special_sign[3:0];
    ex2_special_sel[6:0]  <={ex1_special_sel[8],|ex1_special_sel[7:5],ex1_special_sel[4:0]};
    ex2_special_data[SINGLE_WIDTH-1:0] <= ex1_special_data_final[SINGLE_WIDTH-1:0];
  end
end



assign dp_xx_ex1_srcf2[SINGLE_WIDTH-1:0] = ex1_src2[SINGLE_WIDTH-1:0];
assign dp_xx_ex2_srcf2[SINGLE_WIDTH-1:0] = ex2_special_data[SINGLE_WIDTH-1:0];

assign ex2_inst_wb = (|ex2_special_sel[6:0]) && ex2_freg_vld || falu_fpu_ex2_result_vld;


// &CombBeg; @257
// &CombEnd; @280
// &CombBeg; @285
always @( ex2_special_sel[6:0]
       or ex2_special_data[31:0]
       or ex2_special_sign[3:0])
begin
case(ex2_special_sel[6:0])
  7'b0000_001: ex2_result[SINGLE_WIDTH-1:0]  = { ex2_special_sign[0],ex2_special_data[SINGLE_WIDTH-2:0]};//src2
  7'b0000_010: ex2_result[SINGLE_WIDTH-1:0]  = { ex2_special_sign[1], {31{1'b0}} };//zero
  7'b0000_100: ex2_result[SINGLE_WIDTH-1:0]  = { ex2_special_sign[2], {8{1'b1}},{23{1'b0}} };//inf
  7'b0001_000: ex2_result[SINGLE_WIDTH-1:0]  = { ex2_special_sign[3], {7{1'b1}},1'b0,{23{1'b1}} };//lfn
  7'b0010_000: ex2_result[SINGLE_WIDTH-1:0]  = { 1'b0, {8{1'b1}},1'b1, {22{1'b0}} };//cnan
  7'b0100_000: ex2_result[SINGLE_WIDTH-1:0]  = { ex2_special_data[31],{8{1'b1}}, 1'b1, ex2_special_data[21:0]};//propagate qnan
  7'b1000_000: ex2_result[SINGLE_WIDTH-1:0]  = ex2_special_data[SINGLE_WIDTH-1:0]; //ex1 falu special result
      default: ex2_result[SINGLE_WIDTH-1:0]  = {SINGLE_WIDTH{1'b0}};
endcase
// &CombEnd; @296
end
assign dp_frbus_ex2_data[SINGLE_WIDTH-1:0]  = falu_fpu_ex2_result_vld ? falu_fpu_ex2_result[SINGLE_WIDTH-1:0]: ex2_result[SINGLE_WIDTH-1:0];

assign dp_frbus_ex2_freg[4:0]   = ex2_freg[4:0];
assign dp_frbus_ex2_fflags[4:0] = falu_fpu_ex2_result_vld ? falu_fpu_ex2_fflags[4:0]: ex2_fflags[4:0];
//==========================================================
//                     EX2-EX3 data pipedown
//==========================================================
assign ex3_data_clk_en = ctrl_xx_ex2_inst_vld && !ex2_inst_wb || ctrl_xx_ex2_warm_up;

// &Instance("gated_clk_cell", "x_fpu_data_ex3_gated_clk"); @310
gated_clk_cell  x_fpu_data_ex3_gated_clk (
  .clk_in             (forever_cpuclk    ),
  .clk_out            (ex3_data_clk      ),
  .external_en        (1'b0              ),
  .global_en          (cp0_yy_clk_en     ),
  .local_en           (ex3_data_clk_en   ),
  .module_en          (cp0_fpu_icg_en    ),
  .pad_yy_icg_scan_en (pad_yy_icg_scan_en)
);

// &Connect(.clk_in      (forever_cpuclk      ), @311
//          .external_en (1'b0                ), @312
//          .global_en   (cp0_yy_clk_en       ), @313
//          .module_en   (cp0_fpu_icg_en      ), @314
//          .local_en    (ex3_data_clk_en     ), @315
//          .clk_out     (ex3_data_clk        )); @316

always @(posedge ex3_data_clk)
begin
  if(ctrl_xx_ex2_inst_vld && !ex2_inst_wb && !ctrl_xx_ex2_stall || ctrl_xx_ex2_warm_up)
  begin
    ex3_rm[2:0]      <= ex2_rm[2:0];
    ex3_freg[4:0]    <= ex2_freg[4:0];
  end
end

// &Force("output","ex2_inst_wb"); @342

assign dp_frbus_ex3_freg[4:0]   = ex3_freg[4:0];
assign dp_frbus_ex3_fflags[4:0] = fmau_fpu_ex3_result_vld ? fmau_fpu_ex3_fflags[4:0] : falu_fpu_ex3_fflags[4:0];

assign dp_frbus_ex3_data[SINGLE_WIDTH-1:0]  = fmau_fpu_ex3_result_vld ? fmau_fpu_ex3_result[SINGLE_WIDTH-1:0]
                                                                      : falu_fpu_ex3_result[SINGLE_WIDTH-1:0];

assign ex3_inst_wb = fmau_fpu_ex3_result_vld || falu_fpu_ex3_result_vld;
//==========================================================
//                     EX2-EX3 data pipedown
//==========================================================
assign ex4_data_clk_en = ctrl_xx_ex3_inst_vld && !ex3_inst_wb || ctrl_xx_ex3_warm_up;

// &Instance("gated_clk_cell", "x_fpu_data_ex4_gated_clk"); @364
gated_clk_cell  x_fpu_data_ex4_gated_clk (
  .clk_in             (forever_cpuclk    ),
  .clk_out            (ex4_data_clk      ),
  .external_en        (1'b0              ),
  .global_en          (cp0_yy_clk_en     ),
  .local_en           (ex4_data_clk_en   ),
  .module_en          (cp0_fpu_icg_en    ),
  .pad_yy_icg_scan_en (pad_yy_icg_scan_en)
);

// &Connect(.clk_in      (forever_cpuclk      ), @365
//          .external_en (1'b0                ), @366
//          .global_en   (cp0_yy_clk_en       ), @367
//          .module_en   (cp0_fpu_icg_en      ), @368
//          .local_en    (ex4_data_clk_en     ), @369
//          .clk_out     (ex4_data_clk        )); @370

always @(posedge ex4_data_clk)
begin
  if(ctrl_xx_ex3_inst_vld && !ex3_inst_wb && !ctrl_xx_ex3_stall && !falu_fpu_ex3_wb_gpr || ctrl_xx_ex3_warm_up)
  begin
    ex4_freg[4:0]    <= ex3_freg[4:0];
    ex4_rm[2:0]      <= ex3_rm[2:0];
  end
end

assign dp_frbus_ex4_freg[4:0]   = ex4_freg[4:0];
assign dp_frbus_ex4_fflags[4:0] = fmau_fpu_ex4_fflags[4:0];
assign dp_frbus_ex4_data[SINGLE_WIDTH-1:0]  = fmau_fpu_ex4_result[SINGLE_WIDTH-1:0];

// &ModuleEnd; @406
endmodule



