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

// &ModuleBeg; @23
module pa_fpu_frbus(
  cp0_fpu_icg_en,
  cp0_yy_clk_en,
  cpurst_b,
  ctrl_frbus_ex2_wb_req,
  ctrl_frbus_ex3_wb_req,
  ctrl_frbus_ex4_wb_req,
  ctrl_xx_ex2_warm_up,
  dp_frbus_ex2_data,
  dp_frbus_ex2_fflags,
  dp_frbus_ex2_freg,
  dp_frbus_ex3_data,
  dp_frbus_ex3_fflags,
  dp_frbus_ex3_freg,
  dp_frbus_ex4_data,
  dp_frbus_ex4_fflags,
  dp_frbus_ex4_freg,
  fdsu_frbus_data,
  fdsu_frbus_fflags,
  fdsu_frbus_freg,
  fdsu_frbus_wb_vld,
  forever_cpuclk,
  fpu_cp0_wb_fflags,
  fpu_cp0_wb_fflags_updt,
  fpu_idu_fwd_data,
  fpu_idu_fwd_freg,
  fpu_idu_fwd_vld,
  fpu_rtu_ex3_wb_fflags,
  fpu_rtu_ex3_wb_fflags_vld,
  fpu_rtu_fgpr_wb_data,
  fpu_rtu_fgpr_wb_reg,
  fpu_rtu_fgpr_wb_vld,
  fpu_wb_float_fflags_updt,
  frbus_ctrl_ex2_wb_grant,
  frbus_ctrl_ex3_wb_grant,
  frbus_ctrl_ex4_wb_grant,
  frbus_fdsu_wb_grant,
  ifu_fpu_warm_up,
  pad_yy_icg_scan_en,
  rtu_fpu_ex3_wb_grant,
  rtu_fpu_fgpr_wb_grant
);

// &Ports; @24
input           cp0_fpu_icg_en;           
input           cp0_yy_clk_en;            
input           cpurst_b;                 
input           ctrl_frbus_ex2_wb_req;    
input           ctrl_frbus_ex3_wb_req;    
input           ctrl_frbus_ex4_wb_req;    
input           ctrl_xx_ex2_warm_up;      
input   [31:0]  dp_frbus_ex2_data;        
input   [4 :0]  dp_frbus_ex2_fflags;      
input   [4 :0]  dp_frbus_ex2_freg;        
input   [31:0]  dp_frbus_ex3_data;        
input   [4 :0]  dp_frbus_ex3_fflags;      
input   [4 :0]  dp_frbus_ex3_freg;        
input   [31:0]  dp_frbus_ex4_data;        
input   [4 :0]  dp_frbus_ex4_fflags;      
input   [4 :0]  dp_frbus_ex4_freg;        
input   [31:0]  fdsu_frbus_data;          
input   [4 :0]  fdsu_frbus_fflags;        
input   [4 :0]  fdsu_frbus_freg;          
input           fdsu_frbus_wb_vld;        
input           forever_cpuclk;           
input   [4 :0]  fpu_rtu_ex3_wb_fflags;    
input           fpu_rtu_ex3_wb_fflags_vld; 
input           ifu_fpu_warm_up;          
input           pad_yy_icg_scan_en;       
input           rtu_fpu_ex3_wb_grant;     
input           rtu_fpu_fgpr_wb_grant;    
output  [4 :0]  fpu_cp0_wb_fflags;        
output          fpu_cp0_wb_fflags_updt;   
output  [31:0]  fpu_idu_fwd_data;         
output  [4 :0]  fpu_idu_fwd_freg;         
output          fpu_idu_fwd_vld;          
output  [31:0]  fpu_rtu_fgpr_wb_data;     
output  [4 :0]  fpu_rtu_fgpr_wb_reg;      
output          fpu_rtu_fgpr_wb_vld;      
output          fpu_wb_float_fflags_updt; 
output          frbus_ctrl_ex2_wb_grant;  
output          frbus_ctrl_ex3_wb_grant;  
output          frbus_ctrl_ex4_wb_grant;  
output          frbus_fdsu_wb_grant;      

// &Regs; @25
reg     [4 :0]  fpu_wb_float_fflags;      
reg             fpu_wb_float_fflags_updt; 
reg     [31:0]  frbus_wb_data;            
reg     [4 :0]  frbus_wb_fflags;          
reg     [4 :0]  frbus_wb_freg;            

// &Wires; @26
wire            cp0_fpu_icg_en;           
wire            cp0_yy_clk_en;            
wire            cpurst_b;                 
wire            ctrl_frbus_ex2_wb_req;    
wire            ctrl_frbus_ex3_wb_req;    
wire            ctrl_frbus_ex4_wb_req;    
wire            ctrl_xx_ex2_warm_up;      
wire    [31:0]  dp_frbus_ex2_data;        
wire    [4 :0]  dp_frbus_ex2_fflags;      
wire    [4 :0]  dp_frbus_ex2_freg;        
wire    [31:0]  dp_frbus_ex3_data;        
wire    [4 :0]  dp_frbus_ex3_fflags;      
wire    [4 :0]  dp_frbus_ex3_freg;        
wire    [31:0]  dp_frbus_ex4_data;        
wire    [4 :0]  dp_frbus_ex4_fflags;      
wire    [4 :0]  dp_frbus_ex4_freg;        
wire    [31:0]  fdsu_frbus_data;          
wire    [4 :0]  fdsu_frbus_fflags;        
wire    [4 :0]  fdsu_frbus_freg;          
wire            fdsu_frbus_wb_vld;        
wire            fflags_wb_clk;            
wire            fflags_wb_clk_en;         
wire            forever_cpuclk;           
wire    [4 :0]  fpu_cp0_wb_fflags;        
wire            fpu_cp0_wb_fflags_updt;   
wire            fpu_float_fflags_updt;    
wire    [31:0]  fpu_idu_fwd_data;         
wire    [4 :0]  fpu_idu_fwd_freg;         
wire            fpu_idu_fwd_vld;          
wire            fpu_int_fflags_updt;      
wire    [4 :0]  fpu_rtu_ex3_wb_fflags;    
wire            fpu_rtu_ex3_wb_fflags_vld; 
wire    [31:0]  fpu_rtu_fgpr_wb_data;     
wire    [4 :0]  fpu_rtu_fgpr_wb_fflags;   
wire    [4 :0]  fpu_rtu_fgpr_wb_reg;      
wire            fpu_rtu_fgpr_wb_vld;      
wire    [4 :0]  fpu_wb_fflags;            
wire            fpu_wb_fflags_updt;       
wire    [4 :0]  fpu_wb_int_fflags;        
wire            fpu_wb_int_fflags_updt;   
wire            frbus_ctrl_ex2_wb_grant;  
wire            frbus_ctrl_ex3_wb_grant;  
wire            frbus_ctrl_ex4_wb_grant;  
wire            frbus_ex2_wb_grant;       
wire            frbus_ex2_wb_vld;         
wire            frbus_ex3_wb_grant;       
wire            frbus_ex3_wb_vld;         
wire            frbus_ex4_wb_grant;       
wire            frbus_ex4_wb_vld;         
wire            frbus_fdsu_wb_grant;      
wire            frbus_fdsu_wb_vld;        
wire    [3 :0]  frbus_source_vld;         
wire    [3 :0]  frbus_source_vld_raw;     
wire            frbus_warm_up;            
wire    [3 :0]  frbus_wb_mask;            
wire            frbus_wb_pipedown;        
wire    [3 :0]  frbus_wb_pre;             
wire            frbus_wb_vld;             
wire            ifu_fpu_warm_up;          
wire            pad_yy_icg_scan_en;       
wire            rtu_fpu_ex3_wb_grant;     
wire            rtu_fpu_fgpr_wb_grant;    


parameter FLEN = `FLEN;
//==========================================================
//                   Input Signal Rename
//==========================================================
assign frbus_fdsu_wb_vld = fdsu_frbus_wb_vld;
assign frbus_ex2_wb_vld  = ctrl_frbus_ex2_wb_req;
assign frbus_ex3_wb_vld  = ctrl_frbus_ex3_wb_req;
assign frbus_ex4_wb_vld  = ctrl_frbus_ex4_wb_req;
assign frbus_warm_up     = ctrl_xx_ex2_warm_up;
//assign frbus_wb_pipedown = !rtu_fpu_wb_stall;
assign frbus_wb_pipedown = rtu_fpu_fgpr_wb_grant;

assign frbus_fdsu_wb_grant = frbus_fdsu_wb_vld && !frbus_ex2_wb_vld && !frbus_ex3_wb_vld
                                               && !frbus_ex4_wb_vld && frbus_wb_pipedown;
assign frbus_ex2_wb_grant = frbus_ex2_wb_vld && !frbus_ex3_wb_vld && !frbus_ex4_wb_vld
                                             && frbus_wb_pipedown;
assign frbus_ex3_wb_grant = frbus_ex3_wb_vld && !frbus_ex4_wb_vld && frbus_wb_pipedown;
assign frbus_ex4_wb_grant = frbus_ex4_wb_vld && frbus_wb_pipedown;

assign frbus_wb_pre[3:0]  = {frbus_ex4_wb_vld, frbus_ex3_wb_vld,
                             frbus_ex2_wb_vld, frbus_fdsu_wb_vld};
assign frbus_wb_mask[3:0] = {frbus_ex4_wb_grant, frbus_ex3_wb_grant,
                             frbus_ex2_wb_grant, frbus_fdsu_wb_grant};

assign frbus_source_vld_raw[3:0] = frbus_wb_pre[3:0] & frbus_wb_mask[3:0];
assign frbus_source_vld[3:0]     = frbus_source_vld_raw[3:0] | {2'b0, frbus_warm_up, 1'b0};
assign frbus_wb_vld = |frbus_source_vld_raw[3:0]; // warm up not set wb_vld

// &CombBeg; @56
always @( dp_frbus_ex4_data[31:0]
       or frbus_source_vld[3:0]
       or dp_frbus_ex4_freg[4:0]
       or dp_frbus_ex3_fflags[4:0]
       or dp_frbus_ex3_data[31:0]
       or fdsu_frbus_data[31:0]
       or dp_frbus_ex2_data[31:0]
       or fdsu_frbus_freg[4:0]
       or dp_frbus_ex2_freg[4:0]
       or dp_frbus_ex4_fflags[4:0]
       or fdsu_frbus_fflags[4:0]
       or dp_frbus_ex2_fflags[4:0]
       or dp_frbus_ex3_freg[4:0])
begin
  case(frbus_source_vld[3:0])
    4'b0001: begin // DIV
      frbus_wb_freg[4:0]      = fdsu_frbus_freg[4:0];
      frbus_wb_data[FLEN-1:0] = fdsu_frbus_data[FLEN-1:0];
      frbus_wb_fflags[4:0]    = fdsu_frbus_fflags[4:0];
    end
    4'b0010: begin // EX2
      frbus_wb_freg[4:0]      = dp_frbus_ex2_freg[4:0];
      frbus_wb_data[FLEN-1:0] = dp_frbus_ex2_data[FLEN-1:0];
      frbus_wb_fflags[4:0]    = dp_frbus_ex2_fflags[4:0];
    end
    4'b0100: begin // EX3
      frbus_wb_freg[4:0]      = dp_frbus_ex3_freg[4:0];
      frbus_wb_data[FLEN-1:0] = dp_frbus_ex3_data[FLEN-1:0];
      frbus_wb_fflags[4:0]    = dp_frbus_ex3_fflags[4:0];
    end
    4'b1000: begin // EX4
      frbus_wb_freg[4:0]      = dp_frbus_ex4_freg[4:0];
      frbus_wb_data[FLEN-1:0] = dp_frbus_ex4_data[FLEN-1:0];
      frbus_wb_fflags[4:0]    = dp_frbus_ex4_fflags[4:0];
    end
    default: begin
      frbus_wb_freg[4:0]      = {5{1'bx}};
      frbus_wb_data[FLEN-1:0] = {FLEN{1'bx}};
      frbus_wb_fflags[4:0]    = 5'b0;
    end
  endcase
// &CombEnd; @99
end

//==========================================================
//                  Output Signal Rename
//==========================================================
assign frbus_ctrl_ex2_wb_grant  = frbus_ex2_wb_grant;
assign frbus_ctrl_ex3_wb_grant  = frbus_ex3_wb_grant;
assign frbus_ctrl_ex4_wb_grant  = frbus_ex4_wb_grant;
// &Force("output", "frbus_fdsu_wb_grant"); @107

assign fpu_idu_fwd_vld            = frbus_wb_vld;
assign fpu_idu_fwd_freg[4:0]      = frbus_wb_freg[4:0];
assign fpu_idu_fwd_data[FLEN-1:0] = frbus_wb_data[FLEN-1:0];

// assign fpu_lsu_frbus_data_vld       = frbus_wb_vld;
// assign fpu_lsu_frbus_dest_reg[4:0]  = frbus_wb_freg[4:0];
// assign fpu_lsu_frbus_data[FLEN-1:0] = frbus_wb_data[FLEN-1:0];

assign fpu_rtu_fgpr_wb_vld            = frbus_wb_vld;
// &Force("output", "fpu_rtu_fgpr_wb_vld"); @118
assign fpu_rtu_fgpr_wb_reg[4:0]      = frbus_wb_freg[4:0];
assign fpu_rtu_fgpr_wb_data[FLEN-1:0] = frbus_wb_data[FLEN-1:0];
// &Force("bus", "fpu_rtu_fgpr_wb_data", FLEN-1, 0); @127

assign fpu_rtu_fgpr_wb_fflags[4:0]    = frbus_wb_fflags[4:0];

//==========================================================
//                  Fflags write back
//==========================================================
assign fpu_int_fflags_updt   = rtu_fpu_ex3_wb_grant     &&
                               fpu_rtu_ex3_wb_fflags_vld;

assign fpu_float_fflags_updt = rtu_fpu_fgpr_wb_grant &&
                               fpu_rtu_fgpr_wb_vld;

assign fflags_wb_clk_en      = ifu_fpu_warm_up || 
                               fpu_rtu_fgpr_wb_vld||
                               fpu_wb_fflags_updt ||
                               fpu_float_fflags_updt ||
                               fpu_wb_float_fflags_updt;  

// &Instance("gated_clk_cell", "x_fpu_fflags_wb_gated_clk"); @146
gated_clk_cell  x_fpu_fflags_wb_gated_clk (
  .clk_in             (forever_cpuclk    ),
  .clk_out            (fflags_wb_clk     ),
  .external_en        (1'b0              ),
  .global_en          (cp0_yy_clk_en     ),
  .local_en           (fflags_wb_clk_en  ),
  .module_en          (cp0_fpu_icg_en    ),
  .pad_yy_icg_scan_en (pad_yy_icg_scan_en)
);

// &Connect(.clk_in      (forever_cpuclk      ), @147
//          .external_en (1'b0                ), @148
//          .global_en   (cp0_yy_clk_en       ), @149
//          .module_en   (cp0_fpu_icg_en      ), @150
//          .local_en    (fflags_wb_clk_en     ), @151
//          .clk_out     (fflags_wb_clk        )); @152

//confirm next cycle can be write back
//RTU write gprs in ex3 stage
assign fpu_wb_int_fflags[4:0] = fpu_rtu_ex3_wb_fflags[4:0];
assign fpu_wb_int_fflags_updt = fpu_int_fflags_updt;

// &Force("output","fpu_wb_float_fflags_updt"); @159
//RTU write fgprs next stage after fpgr_wb_vld
always@(posedge fflags_wb_clk or negedge cpurst_b)
begin
    if(!cpurst_b)
      fpu_wb_float_fflags_updt <= 1'b0;
    else if(fpu_float_fflags_updt)
      fpu_wb_float_fflags_updt <= 1'b1;
    else
      fpu_wb_float_fflags_updt <= 1'b0;
end

always@(posedge fflags_wb_clk)
begin
    if(ifu_fpu_warm_up || fpu_float_fflags_updt)
       fpu_wb_float_fflags[4:0] <= fpu_rtu_fgpr_wb_fflags[4:0];
    else
       fpu_wb_float_fflags[4:0] <= fpu_wb_float_fflags[4:0];
end

assign fpu_wb_fflags_updt = fpu_wb_int_fflags_updt || fpu_wb_float_fflags_updt;
assign fpu_wb_fflags[4:0] = ({5{fpu_wb_int_fflags_updt}} & fpu_wb_int_fflags[4:0]) |
                            ({5{fpu_wb_float_fflags_updt}} & fpu_wb_float_fflags[4:0]); 

assign fpu_cp0_wb_fflags_updt = fpu_wb_fflags_updt; 
assign fpu_cp0_wb_fflags[4:0] = fpu_wb_fflags[4:0]; 

// &ModuleEnd; @199
endmodule


