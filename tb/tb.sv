/* Copyright (C) 2018 ETH Zurich, University of Bologna
 * All rights reserved.
 *
 * This code is under development and not yet released to the public.
 * Until it is released, the code is under the copyright of ETH Zurich and
 * the University of Bologna, and may contain confidential and/or unpublished
 * work. Any reuse/redistribution is strictly forbidden without written
 * permission from ETH Zurich.
 *
 * Bug fixes and contributions will eventually be released under the
 * SolderPad open hardware license in the context of the PULP platform
 * (http://www.pulp-platform.org), under the copyright of ETH Zurich and the
 * University of Bologna.
 */

`timescale 1ns/1ps

// typedef enum bit [1:0] {FP16, FP8, VFP16, VFP8} FMT;
// typedef enum bit [2:0] {ADD, SUB, MUL, F2I=3'b100, F2UI, I2F, UI2F} OP;
// typedef enum bit [2:0] {RNE, RTZ, RDN, RUP, RMM} RND;

typedef enum bit [1:0] {FP16, FP8, FP16ALT, FP32} FMT;
typedef enum bit [1:0] {INT8, INT16, INT32, INT64} IFMT;
typedef enum bit [3:0] {ADD, SUB, MUL, DIV, F2I, F2UI, I2F, UI2F, F2F, CPK} OP;
typedef enum bit [2:0] {RNE, RTZ, RDN, RUP, RMM} RND;
typedef enum {NX, UF, OF, DZ, NV} FFLAG; // from LSB to MSB


function signed abs(input signed a);
        if (a>=0)
            abs = a;
        else
            abs = -a;
endfunction

class valid_val
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] val;

    //constraints
    constraint valid_constr {

        // only constrain values, sign random
        val[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans
    }
endclass

class inf_val
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] val;

    //constraints
    constraint inf_constr {

        // only constrain values, sign random
        val[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // infinities
        val[MAN_BITS-1:0] == '0;
    }
endclass

class nan_val
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] val;

    //constraints
    constraint valid_constr {

        // only constrain values, sign random
        val[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // nans
        val[MAN_BITS-1:0] != '0;
    }
endclass

class zero_val
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] val;

    //constraints
    constraint valid_constr {

        // only constrain values, sign random
        val[EXP_BITS+MAN_BITS-1:MAN_BITS] == '0; // zeroes
        val[MAN_BITS-1:0] == '0;
    }
endclass

class stim
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    logic [EXP_BITS+MAN_BITS:0] a, b, c;
    RND           rnd;
    static string name = "stim";
         // logic [3:0] op;
         // logic [1:0] fmt;
         // logic [1:0] fmt2;
         // logic [1:0] ifmt;
         // logic       vec;
endclass

class valid_stim
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] a, b, c;
    rand RND      rnd;
    static string name = "valid_stim";
         // logic [3:0] op;
         // logic [1:0] fmt;
         // logic [1:0] fmt2;
         // logic [1:0] ifmt;
         // logic       vec;

    //constraints
    constraint normal_ops {

        // only constrain values, sign random
        a[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans
        b[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans
        c[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans


        // addends not too far from each other --> those are 'boring' cases
        abs((a[EXP_BITS+MAN_BITS-1:MAN_BITS] + b[EXP_BITS+MAN_BITS-1:MAN_BITS] - 2**(EXP_BITS-1)-1) // product exp
             - c[EXP_BITS+MAN_BITS-1:MAN_BITS]) < 2*MAN_BITS+10; // addend exp
    }

endclass

class nan_stim
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] a, b, c;
    rand RND         rnd;
    rand logic       gena; // whether a is nan
    rand logic       genb; // whether b is nan
    rand logic       genc; // whether c is nan

    static string name = "nan_stim";
         // logic [3:0] op;
         // logic [1:0] fmt;
         // logic [1:0] fmt2;
         // logic [1:0] ifmt;
         // logic       vec;

    //constraints
    constraint nan_constr {

        // only constrain values, sign random
        if (gena || (!gena && !genb && !genc)) {
            a[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // nans
            a[MAN_BITS-1:0] != '0;
        } else
            a[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans

        if(genb) {
            b[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // nans
            b[MAN_BITS-1:0] != '0;
        } else
            b[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans

        if(genc) {
            c[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // nans
            c[MAN_BITS-1:0] != '0;
        } else
            c[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans

        // no other constraints for nan cases
    }

endclass

class inf_stim
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    rand logic [EXP_BITS+MAN_BITS:0] a, b, c;
    rand RND         rnd;
    rand logic       gena; // whether a is inf
    rand logic       genb; // whether b is inf
    rand logic       genc; // whether c is inf
    static string name = "inf_stim";

         // logic [3:0] op;
         // logic [1:0] fmt;
         // logic [1:0] fmt2;
         // logic [1:0] ifmt;
         // logic       vec;

    //constraints
    constraint inf_ops {

        // only constrain values, sign random
        if(gena || (!gena && !genb && !genc)) {
            a[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // inf
            a[MAN_BITS-1:0] == '0;
        } else
            a[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans
        if(genb) {
            b[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // inf
            b[MAN_BITS-1:0] == '0;
        } else
            b[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans
        if(genc) {
            c[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1; // inf
            c[MAN_BITS-1:0] == '0;
        } else
            c[EXP_BITS+MAN_BITS-1:MAN_BITS] != '1; // no infinities / nans

        // no other constraints for inf cases
    }

endclass

class applied
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    stim #(EXP_BITS, MAN_BITS)  stim;
    int unsigned appltime;

    function new();
        stim = new();
    endfunction : new
endclass

class reactio
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    logic [EXP_BITS+MAN_BITS:0] z;
    logic                 [4:0] status;
endclass

class dwreactio
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10
 );
    logic [EXP_BITS+MAN_BITS:0] z;
    logic                 [7:0] status;
endclass


//{RNE, RTZ, RDN, RUP, RMM} RND;
function logic [2:0] dwrnd(input RND rnd);
    case (rnd)
        RNE: return 3'b000;
        RTZ: return 3'b001;
        RDN: return 3'b011;
        RUP: return 3'b010;
        RMM: return 3'b100;

        default : return 3'b000;
    endcase
endfunction : dwrnd

function string statstr(logic [4:0] stat);
    static string unset = "--";
    FFLAG flag;
    string labels[5];
    for (int i = 4; i >=0 ; i--) begin
        flag = FFLAG'(i);
        labels[i] = stat[i] ? flag.name() : unset;
    end
    return {"|",labels[4],"|",labels[3],"|",labels[2],"|",labels[1],"|",labels[0],"|"};
endfunction : statstr

module tb
#(
    parameter int unsigned EXP_BITS = 5,
    parameter int unsigned MAN_BITS = 10,
    parameter DEBUG = 0,
    parameter CHECKFLAGS = 1
 )
(
    // input logic [2:0] RndMode,
    // input logic [3:0] Operation,
    // input logic [1:0] Format,
    // input logic [1:0] Format2,
    // input logic [1:0] IntFmt,
    // input logic       Vectorial
);

    parameter DESIGNNAME = "sFlt16";

    // parameter DEBUG = 1;
    parameter GENTRACE = 1;

    parameter LATENCY = 0;

    parameter NUMINF = 500000;
    parameter NUMNAN = 500000;
    parameter NUMVALID = 2000000;
    int numspecial = 0;


    parameter string REPORTFILE   = {"../simvectors/",DESIGNNAME,".rpt"};
    parameter string TRACEFILE    = {"../simvectors/",DESIGNNAME,"-trace.rpt"};
    parameter string GENTRACEFILE   = "../simvectors/vecSmallFloatUnit-gentrace.rpt";

    int rf,tf;

    localparam T_CLK = 5ns, T_APPL = 500ps, T_RESP = 500ps;
    localparam WIDTH = EXP_BITS+MAN_BITS+1;

    typedef valid_val #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) valid_val_t;
    typedef inf_val #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) inf_val_t;
    typedef nan_val #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) nan_val_t;
    typedef zero_val #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) zero_val_t;

    typedef valid_stim #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) valid_stim_t;
    typedef nan_stim #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) nan_stim_t;
    typedef inf_stim #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) inf_stim_t;

    typedef applied #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) applied_t;
    typedef reactio #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) reactio_t;
    typedef dwreactio #(.EXP_BITS (EXP_BITS), .MAN_BITS (MAN_BITS)) dwreactio_t;

    // import "DPI-C" function void floatOp(input bit[31:0] a,
    //                                                                          input bit[31:0] b,
    //                                                                          input bit[2:0] rnd,
    //                                                                          input bit[1:0] fmt,
    //                                                                          input bit[2:0] op,
    //                                                                          output bit[31:0] z,
    //                                                                          output bit[4:0] status,
    //                                                                          input int DEBUG,
    //                                                                          input string fname);


    logic Clk_C = 1;
    logic Reset_RB = 1;

    logic [WIDTH-1:0] A_D, B_D, C_D, Z_D, ZGold_D;
    RND RoundMode_S;
    // logic [3:0] Operation_S;
    // logic [1:0] Format_S;
    // logic [1:0] Format2_S;
    // logic [1:0] IntFmt_S;
    // logic       VecOp_S;
    logic [4:0] Status_D;
    logic [7:0] StatusGold_D;
    logic       Enable_S;
    logic       Valid_S, ValidGold_S;
    logic       OFBeforeRnd_S;

    logic [WIDTH-1:0] CANQNAN;
    logic [WIDTH-1:0] SIGNAN;
    logic [WIDTH-2:0] MAXABS;

    initial A_D = '0;
    initial B_D = '0;
    initial C_D = '0;
    initial RoundMode_S =   RTZ ;
    // initial Operation_S = '0;
    // initial Format_S =    '0;
    // initial Format2_S =   '0;
    // initial IntFmt_S =    '0;
    // initial VecOp_S =     '0;
    initial Enable_S = 1'b0;

    initial CANQNAN[WIDTH-1] = 1'b0;
    initial CANQNAN[EXP_BITS+MAN_BITS-1:MAN_BITS] = '1;
    initial CANQNAN[MAN_BITS-1] = 1'b1;
    initial CANQNAN[MAN_BITS-2:0] = '0;

    initial SIGNAN[WIDTH-1] = 1'b0;
    initial SIGNAN[EXP_BITS+MAN_BITS-1:MAN_BITS] = '1;
    initial SIGNAN[MAN_BITS-1] = 1'b0;
    initial SIGNAN[MAN_BITS-2:0] = 1;

    initial MAXABS[WIDTH-2:MAN_BITS+1] = '1;
    initial MAXABS[MAN_BITS] = 1'b0;
    initial MAXABS[MAN_BITS-1:0] = '1;

     // check for nan
    function logic isNan(logic [WIDTH-1:0] inp);
        return (inp[WIDTH-2:MAN_BITS] == '1 && inp[MAN_BITS-1:0] != '0);
    endfunction : isNan

    // check for snan
    function logic isSnan(logic [WIDTH-1:0] inp);
        return (inp[WIDTH-2:MAN_BITS] == '1 && inp[MAN_BITS-1] == 1'b0 && inp[MAN_BITS-2:0] != '0);
    endfunction : isSnan

    // check for qnan
    function logic isQnan(logic [WIDTH-1:0] inp);
        return (inp[WIDTH-2:MAN_BITS] == '1 && inp[MAN_BITS-1] == 1'b1);
    endfunction : isQnan

    // check for inf
    function logic isInf(logic [WIDTH-1:0] inp);
        return (inp[WIDTH-2:MAN_BITS] == '1 && inp[MAN_BITS-1:0] == '0);
    endfunction : isInf

    // check for zero
    function logic isZero(logic [WIDTH-1:0] inp);
        return (inp[WIDTH-2:0] == '0);
    endfunction : isZero

    // DWARE does not provide correct NaNs
    function logic [EXP_BITS+MAN_BITS:0] dwsanitize(logic [EXP_BITS+MAN_BITS:0] inp);
        if(inp[EXP_BITS+MAN_BITS-1:MAN_BITS] == '1 && inp[MAN_BITS-1:0] == 1) begin
            return CANQNAN;
        end
        return inp;
    endfunction : dwsanitize

    // NV, DZ, OF, UF, NX
    function logic [4:0] dwstat(logic [7:0] stat, logic [WIDTH-1:0] res, RND rnd, logic OFBeforeRnd, logic [WIDTH-1:0] a, logic [WIDTH-1:0] b, logic [WIDTH-1:0] c);

        automatic logic ofrnd = 1'b0;
        automatic logic qnanvalid = 1'b0;

        automatic logic innan = (isNan(a) || isNan(b) || isNan(c));
        automatic logic insnan = (isSnan(a) || isSnan(b) || isSnan(c));

        // catch the case when designware treats input quiet nans as signalling and raises NV and silence it
        if(innan && !insnan && !((isInf(a) && isZero(b)) || isInf(b) && isZero(a))) begin
            qnanvalid = 1'b1;
        end

        // catch the case when designware rounds down overflow but doesn't signal overflow in the end
        if(OFBeforeRnd && (rnd == RTZ || (rnd == RDN && res[WIDTH-1]==1'b0) || (rnd == RUP && res[WIDTH-1]==1'b1))) begin
            ofrnd = (res[WIDTH-2:0]==MAXABS) & stat[5]; // we have maxnorm and inexact --> we rounded from above maxnorm
        end

        return '{stat[2] & !qnanvalid, 1'b0, stat[4] || stat[1] || ofrnd, stat[3], stat[5] || stat[1]};
    endfunction : dwstat



    // mailbox stim_ch = new();
    // mailbox actresp_ch = new();
    mailbox appl_ch = new();    // application metadata
    mailbox expresp_ch = new(); // expected response

    logic ApplDone = 0;
    logic SimDone = 0;

    int failcounter = 0;
    int statfailcounter = 0;

    int unsigned firstfail = 0;
    int unsigned firstflagfail = 0;


    // vecSmallFloatUnitWrap mut_inst (
    fp_fma_slv
    #(
        .EXP_BITS (EXP_BITS),
        .MAN_BITS (MAN_BITS),
        .LATENCY(0)
     )
    mut_inst
    (
        .Clk_CI       (Clk_C),
        .Reset_RBI    (Reset_RB),
        .A_DI         (A_D),
        .B_DI         (B_D),
        .C_DI         (C_D),
        .RoundMode_SI (RoundMode_S[2:0]),
        .Op_SI(4'h0),
        .OpMod_SI(1'b0),
        // .Op_SI        (Operation_S),
        // .Format_SI    (Format_S),
        // .FPFmt2_SI    (Format2_S),
        // .IntFmt_SI    (IntFmt_S),
        // .VecOp_SI     (VecOp_S),
        .Enable_SI    (Enable_S),
        .Z_DO         (Z_D),
        .Status_DO    (Status_D),
        .Valid_SO     (Valid_S)
    );

    assign OFBeforeRnd_S = tb.mut_inst.i_fp_fma_1.OFBeforeRnd_S;


    DW_fp_mac
    #(
        .sig_width(MAN_BITS),
        .exp_width(EXP_BITS),
        .ieee_compliance(1)
     )
    golden_inst
    (
        .a(A_D),
        .b(B_D),
        .c(C_D),
        .rnd(dwrnd(RoundMode_S)),
        .z(ZGold_D),
        .status(StatusGold_D)
    );


    assign ValidGold_S = Enable_S;

    // Clock Generator
    initial begin
        while (!SimDone)
            #(T_CLK/2) Clk_C = ~Clk_C;
    end

    default clocking cb @(posedge Clk_C);
     default input #T_RESP output #T_APPL;
     input Z_D, Status_D, Valid_S, OFBeforeRnd_S, ZGold_D, StatusGold_D, ValidGold_S;
     // input Z_D, StatusFlags_D, Valid_S;
     output A_D, B_D, C_D, RoundMode_S, Enable_S;
     // output A_D, B_D, C_D, RoundMode_S, Operation_S, Format_S, Format2_S, IntFmt_S, VecOp_S;
    endclocking


    // Application Task
    task apply(logic [WIDTH-1:0] a, logic [WIDTH-1:0] b, logic [WIDTH-1:0] c, RND rnd);

        automatic applied_t appl = new();

        // apply stimuli to both the unit and golden model
        cb.A_D          <= a;
        cb.B_D          <= b;
        cb.C_D          <= c;
        cb.RoundMode_S  <= rnd;
        cb.Enable_S     <= 1'b1;

        // put stim and current time into mailbox
        appl.stim.a = a;
        appl.stim.b = b;
        appl.stim.c = c;
        appl.stim.rnd = rnd;
        appl.appltime = $time;

        appl_ch.put(appl);


        // wait for a clock pulse
        @(cb);

        // disable unit after use (will be overwritten by next stim if present)
        cb.Enable_S <= 1'b0;

    endtask : apply

    // Apply special values: provide one of the keywords to force that operator a predefined value.
    // The non-keyworded operators are random valid inputs.
    // Allowed keywords: "zero"= 0 with random sign
    //                   "+zero" = +0
    //                   "-zero" = -0
    //                   "inf" = inifity with random sign
    //                   "+inf" = positive inifity
    //                   "-inf" = negative inifity
    //                   "nan" = random nan
    //                   "qnan" = canonical quiet nan
    //                   "snan" = signalling nan
    task apply_special(string a, string b, string c, RND rnd);


        automatic applied_t appl = new();

        automatic valid_val_t valid = new();
        automatic zero_val_t zero = new();
        automatic inf_val_t inf = new();
        automatic nan_val_t nan = new();

        logic [WIDTH-1:0] astim, bstim, cstim;
        string astr, bstr, cstr;

        astr = a;
        bstr = b;
        cstr = c;

        // operand A
        if(a=="zero" || a=="+zero" || a=="-zero") begin

            zero.randomize();
            astim = zero.val;

            if (a=="+zero") begin
                astim[WIDTH-1] = 1'b0;
            end else if (a=="-zero") begin
                astim[WIDTH-1] = 1'b1;
            end

        end else if(a=="inf" || a=="+inf" || a=="-inf") begin

            inf.randomize();
            astim = inf.val;

            if (a=="+inf") begin
                astim[WIDTH-1] = 1'b0;
            end else if (a=="-inf") begin
                astim[WIDTH-1] = 1'b1;
            end

        end else if(a=="nan" || a=="qnan" || a=="snan") begin

            nan.randomize();
            astim = nan.val;

            if (a=="qnan") begin
                astim = CANQNAN;
            end else if (a=="snan") begin
                astim = SIGNAN;
            end

        end else begin

            valid.randomize();
            astim = valid.val;

            astr = "fpval";

        end

        // operand B
        if(b=="zero" || b=="+zero" || b=="-zero") begin

            zero.randomize();
            bstim = zero.val;

            if (b=="+zero") begin
                bstim[WIDTH-1] = 1'b0;
            end else if (b=="-zero") begin
                bstim[WIDTH-1] = 1'b1;
            end

        end else if(b=="inf" || b=="+inf" || b=="-inf") begin

            inf.randomize();
            bstim = inf.val;

            if (b=="+inf") begin
                bstim[WIDTH-1] = 1'b0;
            end else if (b=="-inf") begin
                bstim[WIDTH-1] = 1'b1;
            end

        end else if(b=="nan" || b=="qnan" || b=="snan") begin

            nan.randomize();
            bstim = nan.val;

            if (b=="qnan") begin
                bstim = CANQNAN;
            end else if (b=="snan") begin
                bstim = SIGNAN;
            end

        end else begin

            valid.randomize();
            bstim = valid.val;

            bstr = "fpval";

        end

        // operand C
        if(c=="zero" || c=="+zero" || c=="-zero") begin

            zero.randomize();
            cstim = zero.val;

            if (c=="+zero") begin
                cstim[WIDTH-1] = 1'b0;
            end else if (c=="-zero") begin
                cstim[WIDTH-1] = 1'b1;
            end

        end else if(c=="inf" || c=="+inf" || c=="-inf") begin

            inf.randomize();
            cstim = inf.val;

            if (c=="+inf") begin
                cstim[WIDTH-1] = 1'b0;
            end else if (c=="-inf") begin
                cstim[WIDTH-1] = 1'b1;
            end

        end else if(c=="nan" || c=="qnan" || c=="snan") begin

            nan.randomize();
            cstim = nan.val;

            if (c=="qnan") begin
                cstim = CANQNAN;
            end else if (c=="snan") begin
                cstim = SIGNAN;
            end

        end else begin

            valid.randomize();
            cstim = valid.val;

            cstr = "fpval";

        end

        // apply stimuli to both the unit and golden model
        cb.A_D          <= astim;
        cb.B_D          <= bstim;
        cb.C_D          <= cstim;
        cb.RoundMode_S  <= rnd;
        cb.Enable_S     <= 1'b1;

        // put stim and current time into mailbox
        appl.stim.a = astim ;
        appl.stim.b = bstim ;
        appl.stim.c = cstim ;
        appl.stim.rnd = rnd;
        appl.appltime = $time;

        appl_ch.put(appl);

        // count the special cycle
        numspecial = numspecial+1;

        // $display("Special Case: applying fma(%s, %s, %s), %s",astr, bstr, cstr, RND'(rnd));

        // wait for AFTER a clock pulse
        @(cb);

        // disable unit after use (will be overwritten by next stim if present)
        cb.Enable_S <= 1'b0;


    endtask : apply_special


    // run a number of stimuli of a certain type (class)
    task test_nan(int unsigned numLoops);

        automatic nan_stim_t stim = new();

        $display("\n----------------------------------------\nApplying %0d cycles of stimuli from class %s\n----------------------------------------",numLoops,stim.name);

        for (int i=0; i<numLoops; i++) begin

            // randomize stimuli
            while (! stim.randomize()) begin
                $error("rand failed on class %s",stim.name);
            end

            apply(stim.a, stim.b, stim.c, stim.rnd);

            if ((i+1)%10000==0) begin
                $display("\t%0d/%0d [ %0g%% ]",i+1,numLoops,100.0*(i+1)/numLoops);
            end

        end

        $display("\nDONE, applied %0d cycles of stimuli from class %s",numLoops,stim.name);

    endtask : test_nan

    // run a number of stimuli of a certain type (class)
    task test_inf(int unsigned numLoops);

        automatic inf_stim_t stim = new();

        $display("\n----------------------------------------\nApplying %0d cycles of stimuli from class %s\n----------------------------------------",numLoops,stim.name);

        for (int i=0; i<numLoops; i++) begin

            // randomize stimuli
            while (! stim.randomize()) begin
                $error("rand failed on class %s",stim.name);
            end

            apply(stim.a, stim.b, stim.c, stim.rnd);

            if ((i+1)%10000==0) begin
                $display("\t%0d/%0d [ %0g%% ]",i+1,numLoops,100.0*(i+1)/numLoops);
            end

        end

        $display("\nDONE, applied %0d cycles of stimuli from class %s",numLoops,stim.name);

    endtask : test_inf

    task test_valid(int unsigned numLoops);

        automatic valid_stim_t stim = new();

        $display("\n----------------------------------------\nApplying %0d cycles of stimuli from class %s\n----------------------------------------",numLoops,stim.name);

        for (int i=0; i<numLoops; i++) begin

            // randomize stimuli
            while (! stim.randomize()) begin
                $error("rand failed on class %s",stim.name);
            end

            apply(stim.a, stim.b, stim.c, stim.rnd);

            if ((i+1)%10000==0) begin
                $display("\t%0d/%0d [ %0g%% ]",i+1,numLoops,100.0*(i+1)/numLoops);
            end

        end

        $display("\nDONE, applied %0d cycles of stimuli from class %s",numLoops,stim.name);

    endtask : test_valid


    ////////////////////////////
    // Run them tests!
    ////////////////////////////
    initial begin


        // tf = $fopen(TRACEFILE,"w");


        // RESET
        Reset_RB = '0;
        #11ns Reset_RB = '1;
        @(cb)

        // Test some special cases first:
        // Concerning Invalid Ops
        $display("\nTesting Zero Ops");

        apply_special("+zero","+zero","+zero", RNE); // returns 0
        apply_special("+zero","+zero","-zero", RNE); // returns 0
        apply_special("+zero","-zero","+zero", RNE); // returns 0
        apply_special("+zero","-zero","-zero", RNE); // returns 0
        apply_special("-zero","+zero","+zero", RNE); // returns 0
        apply_special("-zero","+zero","-zero", RNE); // returns 0
        apply_special("-zero","-zero","+zero", RNE); // returns 0
        apply_special("-zero","-zero","-zero", RNE); // returns 0
        apply_special("+zero","val","val", RNE); // returns 0
        apply_special("val","+zero","val", RNE); // returns 0
        apply_special("val","val","+zero", RNE); // returns 0

        ##10;
        $display("\nTesting Invalid Ops");

        apply_special("inf","zero","val", RNE); // invalid: ±inf * ±0
        apply_special("zero","inf","val", RNE); // invalid: ±0 * ±inf
        apply_special("inf","inf","val", RNE); // valid: ±inf * ±inf
        apply_special("+inf","+inf","-inf", RNE); // invalid: +inf + -inf
        apply_special("+inf","-inf","+inf", RNE); // invalid: -inf + +inf
        apply_special("+inf","+inf","+inf", RNE); // valid: +inf + +inf
        apply_special("-inf","+inf","-inf", RNE); // valid: -inf + -inf

        ##10;
        // Bulk tests.
        test_nan(500000);
        ##10;
        test_inf(500000);
        ##10;
        test_valid(2000000);

        ApplDone = 1;

    end


        // for (int i=0; i<NUMVECTORS; i++) begin

        //     // stim_ch.get(stim);
        //     automatic valid_stim_t stim = new();
        //     automatic reactio_t expresp_put = new();

        //     stim.rnd   = RoundMode_S;
        //     // stim.op    = Operation;
        //     // stim.fmt   = Format;
        //     // stim.fmt2  = Format2;
        //     // stim.ifmt  = IntFmt;
        //     // stim.vec   = Vectorial;

        //     while (! stim.randomize()) begin
        //         $error("rand failed");
        //     end

        //     // expresp_ch.get(expresp);

        //     // dat clock
        //     ##1;

        //     // Application and clocking
        //     if (i<NUMVECTORS) begin

        //         if ((i+1)%10000==0) begin
        //             $display("Applying Vectors...\t%0d/%0d [ %0g%% ]",i+1,NUMVECTORS,100.0*(i+1)/NUMVECTORS);
        //         end

        //         cb.A_D          <= stim.a;
        //         cb.B_D          <= stim.b;
        //         cb.C_D          <= stim.c;
        //         cb.RoundMode_S  <= stim.rnd;
        //         // // cb.Operation_S  <= stim.op;
        //         // // cb.Format_S     <= stim.fmt;
        //         // // cb.Format2_S    <= stim.fmt2;
        //         // // cb.IntFmt_S     <= stim.ifmt;
        //         // // cb.VecOp_S      <= stim.vec;

        //         if (DEBUG) begin
        //             $display("-[TESTBENCH]----------------------------------------------------");
        //             $display("@%t Applying: Stim A:    %h",$time, stim.a);
        //             $display("@%t           Stim B:    %h",$time, stim.b);
        //             $display("@%t           RoundMode: %b (%s)",$time, stim.rnd, RND'(stim.rnd));
        //             // $display("@%t           Operation: %b (%s)",$time, stim.op, OP'(stim.op));
        //             // $display("@%t           Format:    %b (%s)",$time, stim.fmt, FMT'(stim.fmt));
        //             // $display("@%t           Format 2:  %b (%s)",$time, stim.fmt2, FMT'(stim.fmt2));
        //             // $display("@%t           Int Type:  %b (%s)",$time, stim.ifmt, IFMT'(stim.ifmt));
        //             // $display("@%t           Vectorial: %b",$time, stim.vec);
        //         end

        //         $fwrite(tf,"-[TESTBENCH]----------------------------------------------------\n");
        //         $fwrite(tf,"@%t Applying: Stim A:    %h\n",$time, stim.a);
        //         $fwrite(tf,"@%t           Stim B:    %h\n",$time, stim.b);
        //         $fwrite(tf,"@%t           RoundMode: %b (%s)\n",$time, stim.rnd, RND'(stim.rnd));
        //         // $fwrite(tf,"@%t           Operation: %b (%s)\n",$time, stim.op, OP'(stim.op));
        //         // $fwrite(tf,"@%t           Format:    %b (%s)\n",$time, stim.fmt, FMT'(stim.fmt));
        //         // $fwrite(tf,"@%t           Format 2:  %b (%s)\n",$time, stim.fmt2, FMT'(stim.fmt2));
        //         // $fwrite(tf,"@%t           Int Type:  %b (%s)\n",$time, stim.ifmt, IFMT'(stim.ifmt));
        //         // $fwrite(tf,"@%t           Vectorial: %b\n",$time, stim.vec);

        //         // $display("@%t Calling function",$time);

        //         // floatOp(stim.a, stim.b,
        //         //                 stim.rnd, stim.fmt, stim.op,
        //         //                 expresp_put.z, expresp_put.status,
        //         //                 GENTRACE, GENTRACEFILE);

        //         // assign expresp_put.z = cb.ZGold_D;

        //         // expresp_ch.put(expresp_put);

        //     end
        //     // else begin
        //     //     @(posedge Clk_C);
        //     // end


    // GOLDEN MODEL READOUT TASK
    initial begin

        // holds expected response for mailbox
        automatic dwreactio_t resp_gold = new();

        // wait for reset to be done
        @(posedge Reset_RB);

        while (!SimDone) begin

            // after each clock cycle
            @(cb);

            // check the golden model for a valid output
            if (cb.ValidGold_S) begin

                // collect responses from golden model (combinatorial) and put them in its mailbox
                resp_gold.z <= cb.ZGold_D;
                resp_gold.status <= cb.StatusGold_D;

                expresp_ch.put(resp_gold);

            end
        end
    end


    // Actual Value Receiver and Checker
    initial begin

        // applied input and expected response
        automatic applied_t appl;
        automatic dwreactio_t resp_gold;

        automatic logic[WIDTH-1:0] actresp;
        automatic logic[4:0]       actstatus;
        automatic logic            ovf;

        automatic logic[WIDTH-1:0] expresp;
        automatic logic[4:0]       expstatus;

        automatic int numvectors = 0;

        // wait for reset to be done
        @(posedge Reset_RB);

        while (!SimDone) begin

            // after each clock cycle
            @(posedge Clk_C);

            // check the unit for a valid output
            if (cb.Valid_S) begin

                // get actual response
                actresp = cb.Z_D;
                actstatus = cb.Status_D;

                ovf = cb.OFBeforeRnd_S;

                // fetch expected response
                expresp_ch.get(resp_gold);

                // fetch applied stimuli and time
                appl_ch.get(appl);

                expresp = dwsanitize(resp_gold.z);
                expstatus = dwstat(resp_gold.status, resp_gold.z, appl.stim.rnd, ovf, appl.stim.a, appl.stim.b, appl.stim.c);

                assert(actresp == expresp)
                else begin
                    $display("Appl. Time: %0d ns  ERROR: Expected Z: %h\tActual Z: %h\tWith A=0x%h, B=0x%h, C=0x%h, %s", appl.appltime+T_APPL, expresp, actresp, appl.stim.a, appl.stim.b, appl.stim.c, RND'(appl.stim.rnd));
                    if (!failcounter)
                        firstfail = appl.appltime+T_APPL;
                    failcounter = failcounter+1;
                end

                assert(actstatus == expstatus)
                else begin
                    if (CHECKFLAGS) begin
                        $display("Appl. Time: %0d ns  ERROR: Expected Status: %s\tActual Status: %s\tWith A=0x%h, B=0x%h, C=0x%h, %s, Z=0x%h", appl.appltime+T_APPL, statstr(expstatus), statstr(actstatus), appl.stim.a, appl.stim.b, appl.stim.c, RND'(appl.stim.rnd), actresp);
                    end
                    if (!statfailcounter)
                        firstflagfail = appl.appltime+T_APPL;
                    statfailcounter = statfailcounter+1;
                end

            end

            if(ApplDone && expresp_ch.num() == 0)
                SimDone = 1;
        end


        numvectors = numspecial + NUMVALID + NUMNAN + NUMINF;

        if(failcounter && statfailcounter)
            $display("\nDONE, applied %0d vectors,\t%0d passed [ %0g%% ],\t%0d value checks FAILED! [ %0g%% ] (first at %0d ns),\t%0d status checks FAILED! [ %0g%% ] (first at %0d ns)\n",numvectors,numvectors-failcounter-statfailcounter,100.0*(numvectors-failcounter-statfailcounter)/numvectors,failcounter,100.0*failcounter/numvectors,firstfail,statfailcounter,100.0*statfailcounter/numvectors,firstflagfail);
        else if(failcounter)
            $display("\nDONE, applied %0d vectors,\t%0d passed [ %0g%% ],\t%0d value checks FAILED! [ %0g%% ] (first at %0d ns)\n",numvectors,numvectors-failcounter-statfailcounter,100.0*(numvectors-failcounter-statfailcounter)/numvectors,failcounter,100.0*failcounter/numvectors,firstfail);
        else if(statfailcounter)
            $display("\nDONE, applied %0d vectors,\t%0d passed [ %0g%% ],\t%0d status checks FAILED! [ %0g%% ] (first at %0d ns)\n",numvectors,numvectors-failcounter-statfailcounter,100.0*(numvectors-failcounter-statfailcounter)/numvectors,statfailcounter,100.0*statfailcounter/numvectors,firstflagfail);
        else
            $display("\nDONE, applied %0d vectors,\t%0d PASSED! [ %0g%% ], FMA is penut.\n",numvectors,numvectors-failcounter,100.0);

    end


    //         if (i>LATENCY) begin

    //             automatic logic[WIDTH-1:0] actresp = cb.Z_D;
    //             automatic logic[WIDTH-1:0] expresp = cb.ZGold_D;
    //             // expresp_ch.get(expresp_get);

    //             if (DEBUG) begin

    //                 $display("@%t Expected Z: %h",$time, expresp);
    //                 $display("@%t Actual Z:   %h",$time, actresp);

    //                 // $display("@%t Expected STATUS: %b",$time, expresp.status);
    //                 // $display("@%t Actual STATUS:   %b",$time, cb.StatusFlags_D);

    //                 // $display("@%t Actual VALID:   %b",$time, cb.Valid_S);

    //             end


    //             // $fwrite(tf,"@%t Expected Z: %h\n",$time, expresp.z);
    //             $fwrite(tf,"@%t Actual Z:   %h\n",$time, actresp);
    //             $fwrite(tf,"@%t Expected STATUS: %b\n",$time, expresp);
    //             // $fwrite(tf,"@%t Actual STATUS:   %b\n",$time, cb.StatusFlags_D);

    //             // $fwrite(tf,"@%t Actual VALID:   %b\n",$time, cb.Valid_S);
    //             assert(actresp == expresp)
    //                 else begin
    //                     $error("Expected Z: %h\tActual Z: %h",expresp, actresp);
    //                     failcounter = failcounter+1;
    //                 end
    //         // assert(StatusFlags_D == expresp.status);
    //         end

    //     end

    //     $fclose(tf);
    // //  $finish;
    //     SimDone = 1;

    //     if(failcounter>0)
    //         $display("\nDONE, applied %0d vectors,\t%0d passed [ %0g%% ],\t%0d FAILED! [ %0g%% ]\n",NUMVECTORS,NUMVECTORS-failcounter,100.0*(NUMVECTORS-failcounter)/NUMVECTORS,failcounter,100.0*failcounter/NUMVECTORS);
    //     else
    //         $display("\nDONE, applied %0d vectors,\t%0d PASSED! [ %0g%% ], FMA is penut.\n",NUMVECTORS,NUMVECTORS-failcounter,100.0*(NUMVECTORS-failcounter)/NUMVECTORS);
    // end


    // StimGen
    // initial begin
    //   int f;


    //   f = $fopen(GENTRACEFILE,"w");

    //   for (int i=0; i<NUMVECTORS; i++) begin

    //     automatic valid_stim_t lolstim = new();
    //  //   automatic reactio_t lolresp = new();

    //     lolstim.rnd   = RndMode;
    //     lolstim.op    = Operation;
    //     lolstim.fmt   = Format;
    //     lolstim.fmt2  = Format2;
    //     lolstim.ifmt  = IntFmt;
    //     lolstim.vec   = Vectorial;

    //     if (! lolstim.randomize())
    //     begin
    //       $error("rand failed");
    //     end

    //     if (GENTRACE)
    //       begin
    //         $display("-[STIMGEN]------------------------------------------------------");
    //         $display("@%t Generate: Stim A:    %h",$time, lolstim.a);
    //         $display("@%t           Stim B:    %h",$time, lolstim.b);
    //         $display("@%t           RoundMode: %b (%s)",$time, lolstim.rnd, RND'(lolstim.rnd));
    //         $display("@%t           Operation: %b (%s)",$time, lolstim.op, OP'(lolstim.op));
    //         $display("@%t           Format:    %b (%s)",$time, lolstim.fmt, FMT'(lolstim.fmt));
    //       end

    //       $fwrite(f,"-[STIMGEN]------------------------------------------------------\n");
    //       $fwrite(f,"@%t Generate: Stim A:    %h\n",$time, lolstim.a);
    //       $fwrite(f,"@%t           Stim B:    %h\n",$time, lolstim.b);
    //       $fwrite(f,"@%t           RoundMode: %b (%s)\n",$time, lolstim.rnd, RND'(lolstim.rnd));
    //       $fwrite(f,"@%t           Operation: %b (%s)\n",$time, lolstim.op, OP'(lolstim.op));
    //       $fwrite(f,"@%t           Format:    %b (%s)\n",$time, lolstim.fmt, FMT'(lolstim.fmt));

    //     // floatOp(lolstim.a, lolstim.b,
    //     //         lolstim.rnd, lolstim.fmt, lolstim.op,
    //     //         lolresp.z, lolresp.status,
    //     //         GENTRACE, GENTRACEFILE);

    //     // $fwrite(f,"@%t Expected: Result:    %h\n",$time, lolresp.z);
    //     // $fwrite(f,"@%t           Status:    %b\n",$time, lolresp.status);

    //   //  stim_ch.put(lolstim);
    //  //   expresp_ch.put(lolresp);

    //   end

    //   $fclose(GENTRACEFILE);

    // end

endmodule // tb
