# FPnew - New Floating-Point Unit with Transprecision Capabilities

Parametric floating-point unit with support for standard RISC-V formats and operations as well as transprecision formats.

## IMPORTANT

This project is currently under development. It is not considered stable and there is no support.

## Getting Started

### Dependencies

Consider using [Bender](https://iis-git.ee.ethz.ch/fschuiki/bender) for managing dependencies in your projects. FPnew comes with Bender support!

FPnew currently depends on the following:
- `find_first_one` from the `common_cells` repository (https://iis-git.ee.ethz.ch/sasa/common_cells)
- The divider and square-root unit (if needed) from the `fpu-div-sqrt-mvp` repository (https://iis-git.ee.ethz.ch/pulp-open/fpu-div-sqrt-mvp)

### Usage - VHDL Users

VHDL users should make use of the FPnew package in the instantiating architecture in order to have access to all types used within FPnew.

```VHDL
library work;
use work.fpnew_pkg.all;

```
Then, `fpnew` can be directly instantiated in your design.
```VHDL
i_fpnew: fpnew
generic map (
  FORMATS    => FORMATS,
  INTFORMATS => INTFORMATS,
  UNITTYPES  => UNITTYPES,
  LATENCIES  => LATENCIES,
  GENVECTORS => GENVECTORS,
  TAG_WIDTH  => TAG_WIDTH,
  IN_NANBOX  => IN_NANBOX)
port map (
  Clk_CI         => Clk_CI,
  Reset_RBI      => Reset_RBI,
  A_DI           => A_D,
  B_DI           => B_D,
  C_DI           => C_D,
  RoundMode_SI   => RoundMode_S,
  Op_SI          => Op_S,
  OpMod_SI       => OpMod_S,
  VectorialOp_SI => VectorialOp_S,
  FpFmt_SI       => FpFmt_S,
  FpFmt2_SI      => FpFmt2_S,
  IntFmt_SI      => IntFmt_S,
  Tag_DI         => Tag_D,
  InValid_SI     => InValid_S,
  InReady_SO     => InReady_S,
  Flush_SI       => Flush_S,
  Z_DO           => Z_D,
  Status_DO      => Status_D,
  Tag_DO         => Tag_D,
  OutValid_SO    => OutValid_S,
  OutReady_SI    => OutReady_S);
```

### Usage - SystemVerilog Users

Unfortunately, VHDL packages cannot be imported in SystemVerilog. Thus, `fpnew_top`, a wrapper that converts all ports of `fpnew` to `std_logic_1164` types is provided. Furthermore, it parallelizes array-type generics into scalar ones so instantiation is more convenient in SystemVerilog.
*Note* not all generics are acessible as parameters of `fpnew_top`. If you want to change another generic, it's best to remap it in `fpnew_top` directly or adding a new parameter `fpnew_top`.

Instantiate `fpnew_top` directly in your module.
```SystemVerilog
fpnew_top #(
    .WIDTH                ( WIDTH                ),
    .TAG_WIDTH            ( TAG_WIDTH            ),

    .RV64                 ( RV64                 ),
    .RVF                  ( RVF                  ),
    .RVD                  ( RVD                  ),
    .Xf16                 ( Xf16                 ),
    .Xf16alt              ( Xf16alt              ),
    .Xf8                  ( Xf8                  ),
    .Xfvec                ( Xfvec                ),

    .TYPE_ADDMUL          ( TYPE_ADDMUL          ),
    .TYPE_DIVSQRT         ( TYPE_DIVSQRT         ),
    .TYPE_NONCOMP         ( TYPE_NONCOMP         ),
    .TYPE_CONV            ( TYPE_CONV            ),

    .LATENCY_COMP_F       ( LATENCY_COMP_F       ),
    .LATENCY_COMP_D       ( LATENCY_COMP_D       ),
    .LATENCY_COMP_Xf16    ( LATENCY_COMP_Xf16    ),
    .LATENCY_COMP_Xf16alt ( LATENCY_COMP_Xf16alt ),
    .LATENCY_COMP_Xf8     ( LATENCY_COMP_Xf8     ),
    .LATENCY_DIVSQRT      ( LATENCY_DIVSQRT      ),
    .LATENCY_NONCOMP      ( LATENCY_NONCOMP      ),
    .LATENCY_CONV         ( LATENCY_CONV         )
) i_fpnew_top (
    .Clk_CI         ( clk_i          ),
    .Reset_RBI      ( rst_ni         ),
    .A_DI           ( operand_a      ),
    .B_DI           ( operand_b      ),
    .C_DI           ( operand_c      ),
    .RoundMode_SI   ( rm             ),
    .Op_SI          ( op             ),
    .OpMod_SI       ( op_mod         ),
    .VectorialOp_SI ( vec_op         ),
    .FpFmt_SI       ( fmt            ),
    .FpFmt2_SI      ( fmt2           ),
    .IntFmt_SI      ( ifmt           ),
    .Tag_DI         ( in_tag         ),
    .InValid_SI     ( in_valid       ),
    .InReady_SO     ( in_ready       ),
    .Flush_SI       ( flush          ),
    .Z_DO           ( result         ),
    .Status_DO      ( status         ),
    .Tag_DO         ( out_tag        ),
    .OutValid_SO    ( out_valid      ),
    .OutReady_SI    ( out_ready      )
);
```

In order to access the enumerated types from the `fpnew_pkg` package, a module `fpnew_pkg_constants` is provided that statically drives all values of relevant enumerated types. Using these signals to drive input ports of FPnew ensures that changes to the encoding of enumerators from `fpnew_pkg` take effect in your design too.

`fpnew_pkg_constants` can also be directly instantiated and will be optimized during sythesis.
```SystemVerilog
fpnew_pkg_constants i_fpnew_constants (
    .OP_NUMBITS   ( OP_NUMBITS   ),
    .OP_FMADD     ( OP_FMADD     ),
    .OP_FNMSUB    ( OP_FNMSUB    ),
    .OP_ADD       ( OP_ADD       ),
    .OP_MUL       ( OP_MUL       ),
    .OP_DIV       ( OP_DIV       ),
    .OP_SQRT      ( OP_SQRT      ),
    .OP_SGNJ      ( OP_SGNJ      ),
    .OP_MINMAX    ( OP_MINMAX    ),
    .OP_CMP       ( OP_CMP       ),
    .OP_CLASS     ( OP_CLASS     ),
    .OP_F2I       ( OP_F2I       ),
    .OP_I2F       ( OP_I2F       ),
    .OP_F2F       ( OP_F2F       ),
    .OP_CPK       ( OP_CPK       ),
    .FMT_NUMBITS  ( FMT_NUMBITS  ),
    .FMT_FP32     ( FMT_FP32     ),
    .FMT_FP64     ( FMT_FP64     ),
    .FMT_FP16     ( FMT_FP16     ),
    .FMT_FP8      ( FMT_FP8      ),
    .FMT_FP16ALT  ( FMT_FP16ALT  ),
    .FMT_CUST1    ( FMT_CUST1    ),
    .FMT_CUST2    ( FMT_CUST2    ),
    .FMT_CUST3    ( FMT_CUST3    ),
    .IFMT_NUMBITS ( IFMT_NUMBITS ),
    .IFMT_INT8    ( IFMT_INT8    ),
    .IFMT_INT16   ( IFMT_INT16   ),
    .IFMT_INT32   ( IFMT_INT32   ),
    .IFMT_INT64   ( IFMT_INT64   )
);
```

## Documentation

Documentation on the FPnew is provided in `doc/README.md`.

FPnew source code documentation can be generated with `doxygen` and is especially useful as reference for `fpnew_pkg` types and functions:
```
cd doc
doxygen doxyfile
```
The documentation will be generated at `doc/html/index.html` and can be viewed with your favorite browser.
