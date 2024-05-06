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

#include "headers/cpu.h"
#include "headers/bus.h"
#include <map>
#include <string>


//Constructor
CPU::CPU() {

    instructions = {
		{ "BRK", &CPU::BRK, &CPU::IMM, 7 },{ "ORA", &CPU::ORA, &CPU::IZX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 3 },{ "ORA", &CPU::ORA, &CPU::ZP0, 3 },{ "ASL", &CPU::ASL, &CPU::ZP0, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "PHP", &CPU::PHP, &CPU::IMP, 3 },{ "ORA", &CPU::ORA, &CPU::IMM, 2 },{ "ASL", &CPU::ASL, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "ORA", &CPU::ORA, &CPU::ABS, 4 },{ "ASL", &CPU::ASL, &CPU::ABS, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },
		{ "BPL", &CPU::BPL, &CPU::REL, 2 },{ "ORA", &CPU::ORA, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "ORA", &CPU::ORA, &CPU::ZPX, 4 },{ "ASL", &CPU::ASL, &CPU::ZPX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "CLC", &CPU::CLC, &CPU::IMP, 2 },{ "ORA", &CPU::ORA, &CPU::ABY, 4 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 7 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "ORA", &CPU::ORA, &CPU::ABX, 4 },{ "ASL", &CPU::ASL, &CPU::ABX, 7 },{ "???", &CPU::XXX, &CPU::IMP, 7 },
		{ "JSR", &CPU::JSR, &CPU::ABS, 6 },{ "AND", &CPU::AND, &CPU::IZX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "BIT", &CPU::BIT, &CPU::ZP0, 3 },{ "AND", &CPU::AND, &CPU::ZP0, 3 },{ "ROL", &CPU::ROL, &CPU::ZP0, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "PLP", &CPU::PLP, &CPU::IMP, 4 },{ "AND", &CPU::AND, &CPU::IMM, 2 },{ "ROL", &CPU::ROL, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "BIT", &CPU::BIT, &CPU::ABS, 4 },{ "AND", &CPU::AND, &CPU::ABS, 4 },{ "ROL", &CPU::ROL, &CPU::ABS, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },
		{ "BMI", &CPU::BMI, &CPU::REL, 2 },{ "AND", &CPU::AND, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "AND", &CPU::AND, &CPU::ZPX, 4 },{ "ROL", &CPU::ROL, &CPU::ZPX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "SEC", &CPU::SEC, &CPU::IMP, 2 },{ "AND", &CPU::AND, &CPU::ABY, 4 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 7 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "AND", &CPU::AND, &CPU::ABX, 4 },{ "ROL", &CPU::ROL, &CPU::ABX, 7 },{ "???", &CPU::XXX, &CPU::IMP, 7 },
		{ "RTI", &CPU::RTI, &CPU::IMP, 6 },{ "EOR", &CPU::EOR, &CPU::IZX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 3 },{ "EOR", &CPU::EOR, &CPU::ZP0, 3 },{ "LSR", &CPU::LSR, &CPU::ZP0, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "PHA", &CPU::PHA, &CPU::IMP, 3 },{ "EOR", &CPU::EOR, &CPU::IMM, 2 },{ "LSR", &CPU::LSR, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "JMP", &CPU::JMP, &CPU::ABS, 3 },{ "EOR", &CPU::EOR, &CPU::ABS, 4 },{ "LSR", &CPU::LSR, &CPU::ABS, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },
		{ "BVC", &CPU::BVC, &CPU::REL, 2 },{ "EOR", &CPU::EOR, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "EOR", &CPU::EOR, &CPU::ZPX, 4 },{ "LSR", &CPU::LSR, &CPU::ZPX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "CLI", &CPU::CLI, &CPU::IMP, 2 },{ "EOR", &CPU::EOR, &CPU::ABY, 4 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 7 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "EOR", &CPU::EOR, &CPU::ABX, 4 },{ "LSR", &CPU::LSR, &CPU::ABX, 7 },{ "???", &CPU::XXX, &CPU::IMP, 7 },
		{ "RTS", &CPU::RTS, &CPU::IMP, 6 },{ "ADC", &CPU::ADC, &CPU::IZX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 3 },{ "ADC", &CPU::ADC, &CPU::ZP0, 3 },{ "ROR", &CPU::ROR, &CPU::ZP0, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "PLA", &CPU::PLA, &CPU::IMP, 4 },{ "ADC", &CPU::ADC, &CPU::IMM, 2 },{ "ROR", &CPU::ROR, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "JMP", &CPU::JMP, &CPU::IND, 5 },{ "ADC", &CPU::ADC, &CPU::ABS, 4 },{ "ROR", &CPU::ROR, &CPU::ABS, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },
		{ "BVS", &CPU::BVS, &CPU::REL, 2 },{ "ADC", &CPU::ADC, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "ADC", &CPU::ADC, &CPU::ZPX, 4 },{ "ROR", &CPU::ROR, &CPU::ZPX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "SEI", &CPU::SEI, &CPU::IMP, 2 },{ "ADC", &CPU::ADC, &CPU::ABY, 4 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 7 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "ADC", &CPU::ADC, &CPU::ABX, 4 },{ "ROR", &CPU::ROR, &CPU::ABX, 7 },{ "???", &CPU::XXX, &CPU::IMP, 7 },
		{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "STA", &CPU::STA, &CPU::IZX, 6 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "STY", &CPU::STY, &CPU::ZP0, 3 },{ "STA", &CPU::STA, &CPU::ZP0, 3 },{ "STX", &CPU::STX, &CPU::ZP0, 3 },{ "???", &CPU::XXX, &CPU::IMP, 3 },{ "DEY", &CPU::DEY, &CPU::IMP, 2 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "TXA", &CPU::TXA, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "STY", &CPU::STY, &CPU::ABS, 4 },{ "STA", &CPU::STA, &CPU::ABS, 4 },{ "STX", &CPU::STX, &CPU::ABS, 4 },{ "???", &CPU::XXX, &CPU::IMP, 4 },
		{ "BCC", &CPU::BCC, &CPU::REL, 2 },{ "STA", &CPU::STA, &CPU::IZY, 6 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "STY", &CPU::STY, &CPU::ZPX, 4 },{ "STA", &CPU::STA, &CPU::ZPX, 4 },{ "STX", &CPU::STX, &CPU::ZPY, 4 },{ "???", &CPU::XXX, &CPU::IMP, 4 },{ "TYA", &CPU::TYA, &CPU::IMP, 2 },{ "STA", &CPU::STA, &CPU::ABY, 5 },{ "TXS", &CPU::TXS, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "???", &CPU::NOP, &CPU::IMP, 5 },{ "STA", &CPU::STA, &CPU::ABX, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },
		{ "LDY", &CPU::LDY, &CPU::IMM, 2 },{ "LDA", &CPU::LDA, &CPU::IZX, 6 },{ "LDX", &CPU::LDX, &CPU::IMM, 2 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "LDY", &CPU::LDY, &CPU::ZP0, 3 },{ "LDA", &CPU::LDA, &CPU::ZP0, 3 },{ "LDX", &CPU::LDX, &CPU::ZP0, 3 },{ "???", &CPU::XXX, &CPU::IMP, 3 },{ "TAY", &CPU::TAY, &CPU::IMP, 2 },{ "LDA", &CPU::LDA, &CPU::IMM, 2 },{ "TAX", &CPU::TAX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "LDY", &CPU::LDY, &CPU::ABS, 4 },{ "LDA", &CPU::LDA, &CPU::ABS, 4 },{ "LDX", &CPU::LDX, &CPU::ABS, 4 },{ "???", &CPU::XXX, &CPU::IMP, 4 },
		{ "BCS", &CPU::BCS, &CPU::REL, 2 },{ "LDA", &CPU::LDA, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "LDY", &CPU::LDY, &CPU::ZPX, 4 },{ "LDA", &CPU::LDA, &CPU::ZPX, 4 },{ "LDX", &CPU::LDX, &CPU::ZPY, 4 },{ "???", &CPU::XXX, &CPU::IMP, 4 },{ "CLV", &CPU::CLV, &CPU::IMP, 2 },{ "LDA", &CPU::LDA, &CPU::ABY, 4 },{ "TSX", &CPU::TSX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 4 },{ "LDY", &CPU::LDY, &CPU::ABX, 4 },{ "LDA", &CPU::LDA, &CPU::ABX, 4 },{ "LDX", &CPU::LDX, &CPU::ABY, 4 },{ "???", &CPU::XXX, &CPU::IMP, 4 },
		{ "CPY", &CPU::CPY, &CPU::IMM, 2 },{ "CMP", &CPU::CMP, &CPU::IZX, 6 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "CPY", &CPU::CPY, &CPU::ZP0, 3 },{ "CMP", &CPU::CMP, &CPU::ZP0, 3 },{ "DEC", &CPU::DEC, &CPU::ZP0, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "INY", &CPU::INY, &CPU::IMP, 2 },{ "CMP", &CPU::CMP, &CPU::IMM, 2 },{ "DEX", &CPU::DEX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "CPY", &CPU::CPY, &CPU::ABS, 4 },{ "CMP", &CPU::CMP, &CPU::ABS, 4 },{ "DEC", &CPU::DEC, &CPU::ABS, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },
		{ "BNE", &CPU::BNE, &CPU::REL, 2 },{ "CMP", &CPU::CMP, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "CMP", &CPU::CMP, &CPU::ZPX, 4 },{ "DEC", &CPU::DEC, &CPU::ZPX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "CLD", &CPU::CLD, &CPU::IMP, 2 },{ "CMP", &CPU::CMP, &CPU::ABY, 4 },{ "NOP", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 7 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "CMP", &CPU::CMP, &CPU::ABX, 4 },{ "DEC", &CPU::DEC, &CPU::ABX, 7 },{ "???", &CPU::XXX, &CPU::IMP, 7 },
		{ "CPX", &CPU::CPX, &CPU::IMM, 2 },{ "SBC", &CPU::SBC, &CPU::IZX, 6 },{ "???", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "CPX", &CPU::CPX, &CPU::ZP0, 3 },{ "SBC", &CPU::SBC, &CPU::ZP0, 3 },{ "INC", &CPU::INC, &CPU::ZP0, 5 },{ "???", &CPU::XXX, &CPU::IMP, 5 },{ "INX", &CPU::INX, &CPU::IMP, 2 },{ "SBC", &CPU::SBC, &CPU::IMM, 2 },{ "NOP", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::SBC, &CPU::IMP, 2 },{ "CPX", &CPU::CPX, &CPU::ABS, 4 },{ "SBC", &CPU::SBC, &CPU::ABS, 4 },{ "INC", &CPU::INC, &CPU::ABS, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },
		{ "BEQ", &CPU::BEQ, &CPU::REL, 2 },{ "SBC", &CPU::SBC, &CPU::IZY, 5 },{ "???", &CPU::XXX, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 8 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "SBC", &CPU::SBC, &CPU::ZPX, 4 },{ "INC", &CPU::INC, &CPU::ZPX, 6 },{ "???", &CPU::XXX, &CPU::IMP, 6 },{ "SED", &CPU::SED, &CPU::IMP, 2 },{ "SBC", &CPU::SBC, &CPU::ABY, 4 },{ "NOP", &CPU::NOP, &CPU::IMP, 2 },{ "???", &CPU::XXX, &CPU::IMP, 7 },{ "???", &CPU::NOP, &CPU::IMP, 4 },{ "SBC", &CPU::SBC, &CPU::ABX, 4 },{ "INC", &CPU::INC, &CPU::ABX, 7 },{ "???", &CPU::XXX, &CPU::IMP, 7 },
	};

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

/*--------------------------------------------------------------------
    ===================== Addressing Modes =======================
*-------------------------------------------------------------------*/

void CPU::clock() {
    //This is not a clock accurate emulation.
    //Remaining cycles is initially set to 0 so we can start the instruction, but depending on the instruction, we may me asked to wait (aka call clock())
    //Multiple times before remaining_cycles is 0 again and we can perform the next instruction.
    if (remaining_cycles == 0) {

        //Grab the opcode - which is stored at the memory addrerss pc.
        opcode = read(pc);

        //Read the next byte - this may need to be adjusted later since the next instruction is not necessarily adjacent on the 6502. 
        pc++;

        //Index the table using the opcode for starting number of cycles..
        remaining_cycles = instructions[opcode].cycles;

        //Call to adjust adressing mode and perform opcode function. Adressing mode and opcode return a value because they may need an additional clock cycle.
        uint8_t additional_cycle_addrmode = (this->*instructions[opcode].address_mode)();
        uint8_t additional_cycle_operate = (this->*instructions[opcode].operate)();

        //Additional cycles are captured and added to remaining_cycles count. This prolongs the duration of the instruction.
        remaining_cycles += additional_cycle_addrmode;
        remaining_cycles += additional_cycle_operate; 
    }

    //Decrement remaining cycles on every clock call.
    remaining_cycles--;
}

//Implied - The data is implied by the operation, so we don't really need to do anything. For example, CLC is an implied mode operation, it's implied that this operates on the status register (specifically the carry flag).
uint8_t CPU::IMP() {
    //However, the instruction may be operating on the accumulator, so we are going to fetch it.
    fetched = a;
    return 0;   
}

//Immediate - Data is taken from the byte following the opcode.
uint8_t CPU::IMM() {
    //Set the address where the data we will need for the operation is stored, the data we need is the byte following the opcode (We have already incremented pc to point to this data in clock, but we increment pc after).
    address_ABS = pc;
    pc++;
    return 0;
}

//Zero Page Addressing - the byte of data we need is on page 0.
uint8_t CPU::ZP0() {
    //Get the address of the data we need
    address_ABS = read(pc);
    pc++;
    //Zero out the first byte to get the zeroth page (Bitwise and operation).
    address_ABS &= 0x00FF;
    return 0;
}

//Zero Page Addressing With X register offset - the byte of data we need is on page 0 plus the offset of the value in the X register. Useful for iteration.
uint8_t CPU::ZPX() {
    //Get the address of the data we need
    address_ABS = (read(pc) + x);
    pc++;
    //Zero out the first byte to get the zeroth page (Bitwise and operation).
    address_ABS &= 0x00FF;
    return 0;
}

//Zero Page Addressing With Y register offset - the byte of data we need is on page 0 plus the offset of the value in the Y register. Useful for iteration.
uint8_t CPU::ZPY() {
    //Get the address of the data we need
    address_ABS = (read(pc) + y);
    pc++;
    //Zero out the first byte to get the zeroth page (Bitwise and operation).
    address_ABS &= 0x00FF;
    return 0;
}

//Absolute Addressing - address of data required is fully specified.
uint8_t CPU::ABS() {
    //Example: If we had LDA $1028, the low byte would be 0x1000 and the high byte would be 0x0028. Or - ing these together results in the full 16 bit memory address 0x1028 as needed.

    //Get the low byte
    uint16_t low = read(pc);
    pc++;
    //Get the high byte
    uint16_t high = read(pc);
    pc++;
    //Or the low and high together to get the full address.
    address_ABS &= (high << 8) | low;
    return 0;
}

//Absolute Addressing With X Register Offset- address of data required is fully specified plus an X register offset.
uint8_t CPU::ABX() {
    //Get the low byte
    uint16_t low = read(pc);
    pc++;
    //Get the high byte
    uint16_t high = read(pc);
    pc++;
    //Or the low and high together to get the full address.
    address_ABS &= (high << 8) | low;

    //However, we must check if we are now on a new page. If we are, we may need an additional clock cycle.
    if ((address_ABS & 0xFF00) != (high << 8)) {
        return 1;
    }

    return 0;
}

//Absolute Addressing With Y Register Offset- address of data required is fully specified plus a Y register offset.
uint8_t CPU::ABY() {
    //Get the low byte
    uint16_t low = read(pc);
    pc++;
    //Get the high byte
    uint16_t high = read(pc);
    pc++;
    //Or the low and high together to get the full address.
    address_ABS &= (high << 8) | low;

    //However, we must check if we are now on a new page. If we are, we may need an additional clock cycle.
    if ((address_ABS & 0xFF00) != (high << 8)) {
        return 1;
    }

    return 0;
}

//Indirect Addressing - Pointers
uint8_t CPU::IND() {
    //Get the low byte of the pointer
    uint16_t ptr_low = read(pc);
    pc++;
    //Get the high byte of the pointer
    uint16_t ptr_high = read(pc);
    pc++;
    //Or the low and high together to get the full address.
    uint16_t ptr = (ptr_high << 8) | ptr_low;

    //Simulate end of page bug
    if (ptr_low == 0x00FF) {
        address_ABS = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
    }

    //Get the data at the pointer
    else {
        address_ABS = (read(ptr + 1) << 8) | read(ptr + 0);
    }

    return 0;
}

//Indirect Addressing of Zero Page with X Offset
uint8_t CPU::IZX() {
    uint16_t ptr = read(pc);
    pc++;

    //Get low byte of the pointer and offset by x register.
    uint16_t ptr_low_after_offset = read((uint16_t)(ptr + (uint16_t)x) & 0x00FF);
    //Get high byte of the pointer and offset by x register (Adding one to read next byte of our final address).
    uint16_t ptr_high_after_offset = read((uint16_t)(ptr + (uint16_t)x + 1) & 0x00FF);

    address_ABS = (ptr_high_after_offset << 8) | ptr_low_after_offset;

    return 0;
}

//Indirect Addressing of Zero Page with Y Offset - Performed after address is read from memory.
uint8_t CPU::IZY() {
	uint16_t ptr = read(pc);
	pc++;

	uint16_t address_low = read(ptr & 0x00FF);
	uint16_t address_high = read((ptr + 1) & 0x00FF);

	address_ABS = (address_high << 8) | address_low;
	address_ABS += y;
	
    //We have crossed a page, so an extra clock cycle is required.
	if ((address_ABS & 0xFF00) != (address_high << 8)) {
        return 1;
    }

    return 0;
}

//Relative Addressing Mode - For branching instructions, jump to a nearby address from the branch instruction, specifically less than or equal to 127 memory locations.
uint8_t CPU::REL() {
    address_REL = read(pc);
    pc++;
    //Check for negativity
    if (address_REL & 0x80) {
        address_REL |= 0xFF00;
    }
    return 0;
}

/*--------------------------------------------------------------------
    ======================= Instructions ========================
*-------------------------------------------------------------------*/

//Fetch - fetch data stored at address_ABS.
uint8_t CPU::fetch() {
    //Get our data so long as we are not in implied addressing mode
    if (instructions[opcode].address_mode != &CPU::IMP) {
        fetched = read(address_ABS);
    }
    return fetched;
}

//LDA - Load the accumulator with fetched data.
uint8_t CPU::LDA() {
    fetch();
    a = fetched;
    //Set zero flag if this operation has zeroed out the accumulator.
    setFlag(zero, a == 0x00);
    //Set negative flag if bit 7 of the accumulator is 1 (aka negative).
    setFlag(negative, a == 0x80);
    return 0;
}

//LDX - Load the X register with fetched data.
uint8_t CPU::LDX() {
    fetch();
    x = fetched;
    setFlag(zero, x == 0x00);
    setFlag(negative, x == 0x80);
    return 0;
}

//LDY - Load the Y register with fetched data.
uint8_t CPU::LDY() {
    fetch();
    y = fetched;
    setFlag(zero, y == 0x00);
    setFlag(negative, y == 0x80);
    return 0;
}

//STA - Store data from the accumulator into memory.
uint8_t CPU::STA() {
    write(address_ABS, a);
    return 0;
}

//STX - Store data from the X register into memory.
uint8_t CPU::STX() {
    write(address_ABS, x);
    return 0;
}

//STY - Store data from the Y register into memory.
uint8_t CPU::STY() {
    write(address_ABS, y);
    return 0;
}

//PHA - Push the accumulator.
uint8_t CPU::PHA() {
    //Write the accumulator's contents to 0x100 past the stack pointer.
    write(stkp + 0x100, a);
    stkp--;
    return 0;
}

//PHA - Push the status register.
uint8_t CPU::PHP() {
    //Set break status to 1 before push.
    write(stkp + 0x0100, status | brk | unused);
    setFlag(brk, 0);
    setFlag(unused, 0);
    stkp--;
    return 0;
}

//PLA - Pull data from the stack into the accumulator.
uint8_t CPU::PLA() {
    stkp++;
    a = read(stkp + 0x0100);
    //Check if we read a value of zero.
    setFlag(zero, a == 0x00);
    //Check if we read a negative number.
    setFlag(negative, a & 0x80);
    return 0;
}

//PLP - Pull data from the stack into the status register.
uint8_t CPU::PLP() {
    stkp++;
    status = read(stkp + 0x0100);
    setFlag(unused, 1);
    return 0;
}

//TAX - Transfer data from the accumulator to the X register.
uint8_t CPU::TAX() {
    x = a;
    setFlag(zero, x == 0x00);
    setFlag(negative, x & 0x80);
    return 0;
}

//TAX - Transfer data from the accumulator to the Y register.
uint8_t CPU::TAY() {
    y = a;
    setFlag(zero, y == 0x00);
    setFlag(negative, y & 0x80);
    return 0;
}

//TXA - Transfer data from the X register to the accumulator.
uint8_t CPU::TXA() {
    a = x;
    setFlag(zero, a == 0x00);
    setFlag(negative, a & 0x80);
    return 0;
}

//TYA - Transfer data from the Y register to the accumulator.
uint8_t CPU::TYA() {
    a = y;
    setFlag(zero, a == 0x00);
    setFlag(negative, a & 0x80);
    return 0;
}

//TSX - Transfer data from the stack pointer into the X register.
uint8_t CPU::TSX() {
    x = stkp;
    setFlag(zero, x == 0x00);
    setFlag(negative, x & 0x80);
    return 0;
}

//TXS - Transfer data from the X register to the stack pointer.
uint8_t CPU::TXS() {
    stkp = x;
    setFlag(zero, stkp == 0x00);
    setFlag(negative, stkp & 0x80);
    return 0;
}

//ADC - Add with carry.
uint8_t CPU::ADC() {
    //Fetch data to add
    fetch();

    //Cast elements to 16 bits.
    uint8_t temp = (int16_t)a + (uint16_t)fetched + (uint16_t)getFlag(carry);

    //If temp is greater than 255 (max value of an 8-bit integer) we need to set the carry bit.
    setFlag(carry, temp > 255);

    //Set the zero flag - remember we only care about the lower 8 digits since we have cast to a 16-bit integer.
    setFlag(zero, (temp & 0xFF00) == 0);

    //Set overflow according to the folowing logic.
    setFlag(overflow, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);

    //Set negative flag - again we are working with a 16-bit integer.
    setFlag(negative, (temp & 0x0080));

    //Store the 8 bit result into the acculator.
    a = temp & 0x00FF;

    //May need an additional clock cycle here.
    return 1;
}

//SBC - Subtraction with carry. (A = A - M - (1 - C))
uint8_t CPU::SBC() {
    fetch();

    //Consider that it is possible to rewrite the above formula as A = A + (-M) + 1 + C
    // -M + 1 is just the two's complement of M. Thus we can invert M and utilize our addition method.

    //Let's invert our fetched data.
    uint16_t value = (uint16_t)(~fetched);

	uint16_t temp = (uint16_t)a + value + (uint16_t)getFlag(carry);
	setFlag(carry, temp & 0xFF00);
	setFlag(zero, ((temp & 0x00FF) == 0));
	setFlag(overflow, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
	setFlag(negative, temp & 0x0080);
	a = temp & 0x00FF;
	return 1;
}

//DEC - Decrement memory
uint8_t CPU::DEC() {
	fetch();
	uint8_t temp = fetched - 1;
	write(address_ABS, temp & 0x00FF);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 0;
}

//DEC - Decrement the X register.
uint8_t CPU::DEX()
{
	x--;
	setFlag(zero, x == 0x00);
	setFlag(negative, x & 0x80);
	return 0;
}

//DEY - Decrement the Y register.
uint8_t CPU::DEY()
{
	y--;
	setFlag(zero, y == 0x00);
	setFlag(negative, y & 0x80);
	return 0;
}

//INC - Increment memory
uint8_t CPU::INC() {
	fetch();
	uint8_t temp = fetched + 1;
	write(address_ABS, temp & 0x00FF);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 0;
}

//INX - Increment the X register.
uint8_t CPU::INX()
{
	x++;
	setFlag(zero, x == 0x00);
	setFlag(negative, x & 0x80);
	return 0;
}

//INY - Increment the Y register.
uint8_t CPU::INY()
{
	y++;
	setFlag(zero, y == 0x00);
	setFlag(negative, y & 0x80);
	return 0;
}

//ASL - Arithmetic Shift Left, shifts all bits left one position. 0 is shifted into bit 0 and the original bit 7 is shifted into the carry.
uint8_t CPU::ASL() {
    fetch();
    uint16_t temp = (uint16_t)fetched << 1;
    setFlag(carry, (temp & 0xFF00) > 0);
	setFlag(zero, (temp & 0x00FF) == 0x00);
	setFlag(negative, temp & 0x0080);
    //Set accumulator to temp if we are using implied addressing
	if (instructions[opcode].address_mode == &CPU::IMP) {
		a = temp & 0x00FF;
    }

    //otherwise write to memory
	else {
        write(address_ABS, temp & 0x00FF);
    }

	return 0;
}

//ROL - Rotate Left, Shift all bits in fetched data left, storing the 7th bit into the carry and the carry into bit 0.
uint8_t CPU::ROL() {
    fetch();
	uint16_t temp = (uint16_t)(fetched << 1) | getFlag(carry);
	setFlag(carry, temp & 0xFF00);
    setFlag(zero, (temp & 0x00FF) == 0x0000);
    setFlag(negative, temp & 0x0080);

    //Set accumulator to temp if we are using implied addressing
	if (instructions[opcode].address_mode == &CPU::IMP) {
		a = temp & 0x00FF;
    }
    //otherwise write to memory
	else {
        write(address_ABS, temp & 0x00FF);
    }

	return 0;
}

//LSR - Logical Shift Right, shifts all bits right one position. Bit 0 is shifted into bit 7 and the original bit 0 is shifted into the carry.
uint8_t CPU::LSR() {
    fetch();
	setFlag(carry, fetched & 0x0001);
	uint16_t temp = fetched >> 1;	
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);

    //Set accumulator to temp if we are using implied addressing
	if (instructions[opcode].address_mode == &CPU::IMP) {
		a = temp & 0x00FF;
    }
    //otherwise write to memory
	else {
        write(address_ABS, temp & 0x00FF);
    }

	return 0;
}

//ROR - Rotate right, shifts all bits right one position. The Carry is shifted into bit 7 and the original bit 0 is shifted into the Carry.
uint8_t CPU::ROR() {
	fetch();
	uint16_t temp = (uint16_t)(getFlag(carry) << 7) | (fetched >> 1);
	setFlag(carry, fetched & 0x01);
	setFlag(zero, (temp & 0x00FF) == 0x00);
	setFlag(negative, temp & 0x0080);

    //Set accumulator to temp if we are using implied addressing
	if (instructions[opcode].address_mode == &CPU::IMP) {
		a = temp & 0x00FF;
    }
    //otherwise write to memory
	else {
        write(address_ABS, temp & 0x00FF);
    }

	return 0;
}

//AND - Bitwise and the accumulator with fetched data.
uint8_t CPU::AND() {
    fetch();
    //And the data in the accumulator with the fetched data.
    a &= fetched;
    //Set zero flag if this operation has zeroed out the accumulator.
    setFlag(zero, a == 0x00);
    //Set negative flag if bit 7 of the accumulator is 1 (aka negative).
    setFlag(negative, a == 0x80);
    //Potentially may require an extra clock cycle if page is crossed, but this is checked in addressing mode. Recall that our clock function will add another
    //cycle if both the instruction and addressing mode call for another cycle.
    return 1;
}

//EOR - Bitwise XOR the accumulator with fetched data.
uint8_t CPU::EOR() {
    fetch();
    //XOR the data in the accumulator with the fetched data.
    a ^= fetched;
    //Set zero flag if this operation has zeroed out the accumulator.
    setFlag(zero, a == 0x00);
    //Set negative flag if bit 7 of the accumulator is 1 (aka negative).
    setFlag(negative, a == 0x80);
    //Potentially may require an extra clock cycle if page is crossed, but this is checked in addressing mode. Recall that our clock function will add another
    //cycle if both the instruction and addressing mode call for another cycle.
    return 1;
}

//ORA - Bitwise OR the accumulator with fetched data.
uint8_t CPU::ORA() {
    fetch();
    //OR the data in the accumulator with the fetched data.
    a |= fetched;
    //Set zero flag if this operation has zeroed out the accumulator.
    setFlag(zero, a == 0x00);
    //Set negative flag if bit 7 of the accumulator is 1 (aka negative).
    setFlag(negative, a == 0x80);
    //Potentially may require an extra clock cycle if page is crossed, but this is checked in addressing mode. Recall that our clock function will add another
    //cycle if both the instruction and addressing mode call for another cycle.
    return 1;
}

//CMP - Compare accumulator, sets flags as if a subtraction had been carried out.
uint8_t CPU::CMP() {
	fetch();
    uint16_t temp = (uint16_t)a - (uint16_t)fetched;
	setFlag(carry, a >= fetched);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 1;
}

//CPX - Compare X register, sets flags as if a subtraction had been carried out.
uint8_t CPU::CPX() {
	fetch();
    uint16_t temp = (uint16_t)x - (uint16_t)fetched;
	setFlag(carry, x >= fetched);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 1;
}

//CPY - Compare Y register, sets flags as if a subtraction had been carried out.
uint8_t CPU::CPY() {
	fetch();
    uint16_t temp = (uint16_t)y - (uint16_t)fetched;
	setFlag(carry, y >= fetched);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 1;
}

//BIT - Bit Test, places bit 7 of the operand into the N flag and bit 6 of the operand into the V flag. 
//The operand is then ANDed with the accumulator, and the Z flag is set if the result is zero.
uint8_t CPU::BIT() {
    fetch();
    setFlag(negative, fetched & 0x80);
    setFlag(overflow, fetched & 0x40);
	uint16_t temp = a & fetched;
	setFlag(zero, (temp & 0x00FF) == 0x00);
    return 0;
}

//JMP - Jump to next address by modifying the PC.
uint8_t CPU::JMP() {
    pc = address_ABS;
	return 0;
}

//JSR - jump to subroutine (pushes PC on stack, loads operand into PC)
uint8_t CPU::JSR() {
    pc--;

	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	pc = address_ABS;
	return 0;
}

//RTS - Pulls the top two bytes off the stack (low byte first) and transfers program control to that address + 1
uint8_t CPU::RTS() {

    stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;

    pc++;
    return 0;
}

//BCS - Branch on carry clear
uint8_t CPU::BCC() {
    //Carry is clear
    if (getFlag(carry) == 0)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//BCS - Branch if the carry bit of the status register is set.
uint8_t CPU::BCS() {
    //If the carry bit is set
    if (getFlag(carry) == 1) {
        //change address to the relative address from relative addressing, which is initialized as 0
        address_ABS = pc + address_REL;
        //Add 1 to the remaining cycles
        remaining_cycles++;

        //If our branch crosses a page, add another tick to our cycles.
        if ((address_ABS && 0xFF00) != (pc & 0xFF00)) {
            remaining_cycles++;
        }

        //Set location of next instruction after branch
        pc = address_ABS;
    }
    
    return 0;
}

//BEQ - Branch if equals.
uint8_t CPU::BEQ() {
    //Zero flag indicates we have made an equivalent comaparison
    if (getFlag(zero) == 1)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//BMI - Branch if minus.
uint8_t CPU::BMI() {
    //Negative flag is set
    if (getFlag(negative) == 1)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//BNE - Branch if not equals.
uint8_t CPU::BNE() {
    //Zero flag not set indicates our comaprison was not equivalent
    if (getFlag(zero) == 0)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//BPL - Branch if plus.
uint8_t CPU::BPL() {
    //Negative flag is not set
    if (getFlag(negative) == 0)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//BVC - Branch if not overflow.
uint8_t CPU::BVC() {
    //Overflow flag is not set
    if (getFlag(overflow) == 0)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//BVS - Branch if overflow.
uint8_t CPU::BVS() {
    //Overflow flag is set
    if (getFlag(overflow) == 1)
	{
		remaining_cycles++;
		address_ABS = pc + address_REL;
		
		if((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;
		
		pc = address_ABS;
	}
	return 0;
}

//CLC - Clear carry flag
uint8_t CPU::CLC()
{
	setFlag(carry, 0);
	return 0;
}

//CLD - Clear decimal flag
uint8_t CPU::CLD()
{
	setFlag(decimal, 0);
	return 0;
}

//CLI - Clear interrupt flag
uint8_t CPU::CLI()
{
	setFlag(disable, 0);
	return 0;
}

//CLV - Clear overflow flag
uint8_t CPU::CLV()
{
	setFlag(overflow, 0);
	return 0;
}

//SEC - Set carry flag
uint8_t CPU::SEC()
{
	setFlag(carry, 1);
	return 0;
}

//SED - Set decimal flag
uint8_t CPU::SED()
{
	setFlag(decimal, 1);
	return 0;
}

//SED - Set interrupt disable
uint8_t CPU::SEI()
{
	setFlag(disable, 1);
	return 0;
}

//BRK - Break, causes a non maskable interrupt and increments the program counter by one.
uint8_t CPU::BRK() {
    pc++;
	
	setFlag(disable, 1);
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	setFlag(brk, 1);
	write(0x0100 + stkp, status);
	stkp--;
	setFlag(brk, 0);

	pc = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);
	return 0;
}

//RTI - Return from interrupt, the status register is pulled with the break flag and bit 5 ignored. Then PC is pulled from the stack.
uint8_t CPU::RTI()
{
	stkp++;
	status = read(0x0100 + stkp);
	status &= ~brk;
	status &= ~unused;

	stkp++;
	pc = (uint16_t)read(0x0100 + stkp);
	stkp++;
	pc |= (uint16_t)read(0x0100 + stkp) << 8;
	return 0;
}

uint8_t CPU::NOP()
{
	switch (opcode) {
	case 0x1C:
	case 0x3C:
	case 0x5C:
	case 0x7C:
	case 0xDC:
	case 0xFC:
		return 1;
		break;
	}
	return 0;
}

void CPU::reset() {
    //Set the following to a known state
    a = 0;
    x = 0;
    y = 0;
    stkp = 0xFD;
    status = 0x00 | unused;
    
    //Get the address to set the pc to - this is a default address for the 6502.
    address_ABS = 0xFFFC;
    uint16_t high = read(address_ABS + 1);
    uint16_t low = read(address_ABS + 0);
    pc =  (high << 8) | low;

    address_REL = 0x0000;
    address_ABS = 0x0000;
    fetched = 0x00;

    remaining_cycles = 8;
}

void CPU::interrupt() {
    
    //interrupts are not disabled
    if (getFlag(disable) == 0) {
        //Write the current pc (two writes because the pc is 16 bits)
        write(0x0000 + stkp, (pc >> 8) & 0x00FF);
        stkp--;
        write(0x0000 + stkp, pc & 0x00FF);
        stkp--;

        //Write the status register to the stack
        setFlag(unused, 1);
        setFlag(disable, 1);
        setFlag(brk, 0);
        write(0x0100 + stkp, status);
        stkp--;

        //Default address for pc, where programmers are meant to store code serving the interrupt.
        address_ABS = 0xFFFE;
        uint16_t high = read(address_ABS + 1);
        uint16_t low = read(address_ABS + 0);
        pc =  (high << 8) | low;

        remaining_cycles = 8;
    }
}

//Non Maskable interrupt, we dont check the disabled flag
void CPU::non_maskable_interrupt() {
    //Write the current pc (two writes because the pc is 16 bits)
    write(0x0000 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0000 + stkp, pc & 0x00FF);
    stkp--;

    //Write the status register to the stack
    setFlag(unused, 1);
    setFlag(disable, 1);
    setFlag(brk, 0);
    write(0x0100 + stkp, status);
    stkp--;

    //Default address for pc for non maskable interrupts, where programmers are meant to store code serving the interrupt.
    //Note it is not the same address as for regular interrupts.
    address_ABS = 0xFFFA;
    uint16_t high = read(address_ABS + 1);
    uint16_t low = read(address_ABS + 0);
    pc =  (high << 8) | low;

    remaining_cycles = 8;
}

// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
std::map<uint16_t, std::string> CPU::disassemble(uint16_t nStart, uint16_t nStop)
{
	uint32_t addr = nStart;
	uint8_t value = 0x00, lo = 0x00, hi = 0x00;
	std::map<uint16_t, std::string> mapLines;
	uint16_t line_addr = 0;

	// A convenient utility to convert variables into
	// hex strings because "modern C++"'s method with 
	// streams is atrocious
	auto hex = [](uint32_t n, uint8_t d)
	{
		std::string s(d, '0');
		for (int i = d - 1; i >= 0; i--, n >>= 4)
			s[i] = "0123456789ABCDEF"[n & 0xF];
		return s;
	};

	// Starting at the specified address we read an instruction
	// byte, which in turn yields information from the lookup table
	// as to how many additional bytes we need to read and what the
	// addressing mode is. I need this info to assemble human readable
	// syntax, which is different depending upon the addressing mode

	// As the instruction is decoded, a std::string is assembled
	// with the readable output
	while (addr <= (uint32_t)nStop)
	{
		line_addr = addr;

		// Prefix line with instruction address
		std::string sInst = "$" + hex(addr, 4) + ": ";

		// Read instruction, and get its readable name
		uint8_t opcode = bus->read(addr, true); addr++;
		sInst += instructions[opcode].name + " ";

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		if (instructions[opcode].address_mode == &CPU::IMP)
		{
			sInst += " {IMP}";
		}
		else if (instructions[opcode].address_mode == &CPU::IMM)
		{
			value = bus->read(addr, true); addr++;
			sInst += "#$" + hex(value, 2) + " {IMM}";
		}
		else if (instructions[opcode].address_mode == &CPU::ZP0)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;												
			sInst += "$" + hex(lo, 2) + " {ZP0}";
		}
		else if (instructions[opcode].address_mode == &CPU::ZPX)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;														
			sInst += "$" + hex(lo, 2) + ", X {ZPX}";
		}
		else if (instructions[opcode].address_mode == &CPU::ZPY)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;														
			sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
		}
		else if (instructions[opcode].address_mode == &CPU::IZX)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;								
			sInst += "($" + hex(lo, 2) + ", X) {IZX}";
		}
		else if (instructions[opcode].address_mode == &CPU::IZY)
		{
			lo = bus->read(addr, true); addr++;
			hi = 0x00;								
			sInst += "($" + hex(lo, 2) + "), Y {IZY}";
		}
		else if (instructions[opcode].address_mode == &CPU::ABS)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
		}
		else if (instructions[opcode].address_mode == &CPU::ABX)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
		}
		else if (instructions[opcode].address_mode == &CPU::ABY)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
		}
		else if (instructions[opcode].address_mode == &CPU::IND)
		{
			lo = bus->read(addr, true); addr++;
			hi = bus->read(addr, true); addr++;
			sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
		}
		else if (instructions[opcode].address_mode == &CPU::REL)
		{
			value = bus->read(addr, true); addr++;
			sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
		}

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] = sInst;
	}

	return mapLines;
}

bool CPU::complete()
{
	return remaining_cycles == 0;
}