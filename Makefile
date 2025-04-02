MODULE = fpnew_top
SV_SRC = src/common_cells/src/cf_math_pkg.sv src/common_cells/src/lzc.sv src/common_cells/src/rr_arb_tree.sv src/fpnew_pkg.sv $(filter-out src/fpnew_pkg.sv,src/*.sv)
CC_SRC = csrc/*.cpp         
SV_DIR = ./src/common_cells/include

# src/fpnew_pkg.sv $(filter-out src/fpnew_pkg.sv,src/*.sv)

.PHONY: sim
sim: waveform.vcd

.PHONY: vld
vld: .stamp.verilate
	@echo "\n### Verilator编译完成 ###"

.PHONY: wave
wave: waveform.vcd
	sudo gtkwave $<

.PHONY: lint
lint:
	verilator --lint-only $(SV_SRC) -I$(SV_DIR)

waveform.vcd: ./obj_dir/V$(MODULE)
	@echo "\n### 开始仿真 ###"
	@./obj_dir/V$(MODULE)

./obj_dir/V$(MODULE): .stamp.verilate
	@echo "\n### 构建仿真程序 ###"
	$(MAKE) -C obj_dir -f V$(MODULE).mk

.stamp.verilate: $(SV_SRC) $(CC_SRC)
	@echo "\n### 生成Verilator代码 ###"
	verilator -Wno-fatal --trace --x-assign unique --x-initial unique -cc \
		$(SV_SRC) \
		--exe $(CC_SRC) \
		-I$(SV_DIR) \
		--top-module fpnew_top 
	@touch $@

.PHONY: clean
clean:
	rm -rf .stamp.*
	rm -rf ./obj_dir
	rm -rf waveform.vcd
	rm -rf *.log *.vcd


# --public-flat-rw \
# --trace-structs \
# --trace-depth 5 \