#include <verilated.h>
#include "verilated_vcd_c.h"
#include "VVaquitaTop.h"  // This is the class that corresponds to your top-level Verilog module

#define MAX_SIM_TIME 1000  // Max simulation time (in cycles)

int main(int argc, char **argv, char **env) {
    if (false && argc && argv && env) {}

	Verilated::mkdir("logs");

	const std::unique_ptr<VerilatedContext> contextp {new VerilatedContext};
	contextp->commandArgs(argc, argv);
	contextp->traceEverOn(true);

	VerilatedVcdC *tfp = new VerilatedVcdC;

	const std::unique_ptr<VVaquitaTop> top {
		new VVaquitaTop {contextp.get(), "VaquitaTOP"}
	};
	top->trace(tfp, 5);

	tfp->open("logs/vaquitatop.vcd");

	unsigned int sim_time = 0;
	top->clock = 1;
	while (!contextp->gotFinish() && sim_time < MAX_SIM_TIME) {
		top->clock ^= 1;  // Toggle clock
		if (sim_time <= 1) {
		    top->reset = 1;  // Assert reset
		} else {
		    top->reset = 0;  // Deassert reset
		}

        // Apply the test vectors (poke signals)
        // Cycle 1
        if (sim_time == 2) {
            top->io_instr = 0x0222b1d7;  // Set instruction
            top->io_rs1_data = 5;        // Set rs1_data
            top->io_hazard_rs1_data_in = 0;  // Set hazard input
        }

        // Cycle 2
        if (sim_time == 4) {
            top->io_instr = 0x0240b2d7;
            top->io_rs1_data = 0;
        }

        // Cycle 3
        if (sim_time == 8) {
            top->io_instr = 0x02328457;
            top->io_rs1_data = 0;
        }

        // Cycle 4
        if (sim_time == 16) {
            top->io_instr = 0x4a8110d7;
            top->io_rs1_data = 0;
        }

        // Evaluate module state
        top->eval();  // Evaluate model
		tfp->dump(sim_time);
        ++sim_time;

    }
    // Finalize the simulation and dump the waveform
	top->final();
    tfp->close();
    return 0;
} 