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
    //Get the data at the pointer
    address_ABS = (read(ptr + 1) << 8) | read(ptr + 0);

    return 0;
}