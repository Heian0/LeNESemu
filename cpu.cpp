// Address Output (16 - bit wide address)
// Data Port (exchanges 8 bits at a time aka 1 byte at a time)
// Read/Write signal for Data Port
// Clock - evaluate on clock high
// No intrisic understanding

// Need to connect out CPU to the bus via address lines and data lines. 
// When the CPU sends an address to the bus, it expects other devices to respond (on clock high).
// Devices may respond by either putting some data on the bus so the CPU can read it or by or accepting the data that the CPU has sent to the bus.
// This direction is governed by the read/write signal.
// Devices which are connected to the bus need to have an awareness of what addresses they should handle.
// Addresses outputed by the CPU must map to at least one connected device.

// Consider a device on the bus covering the some amount of memory, our RAM.
// In the 6502, we have 64kb of RAM.
// This RAM will store our program variables and the program itself. (Von Neumann Architecture)
// Most of the time the CPU is reading from RAM, but it will also write occasionally.

// On our CPU, we have 16 address pins, A0 - A15 for our address output.
// We also have 8 data pins, D0 - D7, for communication with the bus and various devices.
// We also have a R/W pin.

// We have 3 primary registers, which are all 8 bit.
// The first is the A register, the accumulator.
// We then have the X register.
// And the Y register.
// Functionally they are quite similar, they store a 8-bit word.

// We also have a stack pointer (stkp).
// And a program counter (pc).
// And a status register (status).

// The status register contains various bits representing information the state of the CPU.
// For example, it might tell us if the last result was equal to zero or if there was previously a carry operation.
// We can also use the status register to tell the CPU to enable/disable interrupts.

// Instuctions sent to the 6502 CPU are not all the same length. Some are 1 byte, others 2 bytes, and others 3.
// Thus the program counter cannot simply be incremented per instruction.
// We will need several clock cycles to fetch instructions, as each instruction could be a different length.
// Different instructions take different amounts of clock cycles to complete.
// Thus for each instruction, we will need to be aware of the size of the instruction, and its duration.
// The 6502 has 56 legal instructions.
// These instructions can be mutated to change their size and duration depending on the arguments of the instruction.
// Luckily the first byte of each instruction contains this data.

// LDA $41 has two bytes of instruction. (8 bits in a byte, LDA is load accumulator)
// LDA $0105 has three bytes of instruction. (Load accumulator from memory address 0105, remember memory is 16-bit, which checks out)
// CLC is a one byte instruction. (Clear carry bit in the status register)

// So to run an instruction, we will first read the first byte at the (pc) whcih  which is an opcode.
// We will use this opcode to index the instruction data table (which will be stored in the form of an array).
// Now we can access the addressing mode and the number of cycles for this instruction.
// Now that we have the addressing mode, we can read 1, 2, or 3 more bytes at the (pc), and we can execute the instruction.
// Then we can wait (count clock cycles) until the operation is complete.

#include "cpu.h"
#include "bus.h"

//Constructor
CPU::CPU() {

}

//Destructor
CPU::~CPU() {

}

void CPU::write(uint16_t address, uint8_t data) {
    //Calling the bus's write function
    bus->write(address, data);
}

uint8_t CPU::read(uint16_t address) {
    //Calling the bus's read function, default readonly as false.
    return bus->read(address, false);
}