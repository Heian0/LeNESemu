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

#include "bus.h"
#include <map>
#include <string>
#include <cstdint>
#include <iostream>


//Constructor
CPU::CPU() {

	using a = CPU;
	instructions =
	{
		{ "BRK", &a::BRK, &a::IMM, 7 },{ "ORA", &a::ORA, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::ZP0, 3 },{ "ASL", &a::ASL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHP", &a::PHP, &a::IMP, 3 },{ "ORA", &a::ORA, &a::IMM, 2 },{ "ASL", &a::ASL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABS, 4 },{ "ASL", &a::ASL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BPL", &a::BPL, &a::REL, 2 },{ "ORA", &a::ORA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ZPX, 4 },{ "ASL", &a::ASL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLC", &a::CLC, &a::IMP, 2 },{ "ORA", &a::ORA, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ORA", &a::ORA, &a::ABX, 4 },{ "ASL", &a::ASL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "JSR", &a::JSR, &a::ABS, 6 },{ "AND", &a::AND, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "BIT", &a::BIT, &a::ZP0, 3 },{ "AND", &a::AND, &a::ZP0, 3 },{ "ROL", &a::ROL, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLP", &a::PLP, &a::IMP, 4 },{ "AND", &a::AND, &a::IMM, 2 },{ "ROL", &a::ROL, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "BIT", &a::BIT, &a::ABS, 4 },{ "AND", &a::AND, &a::ABS, 4 },{ "ROL", &a::ROL, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BMI", &a::BMI, &a::REL, 2 },{ "AND", &a::AND, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ZPX, 4 },{ "ROL", &a::ROL, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEC", &a::SEC, &a::IMP, 2 },{ "AND", &a::AND, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "AND", &a::AND, &a::ABX, 4 },{ "ROL", &a::ROL, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTI", &a::RTI, &a::IMP, 6 },{ "EOR", &a::EOR, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "EOR", &a::EOR, &a::ZP0, 3 },{ "LSR", &a::LSR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PHA", &a::PHA, &a::IMP, 3 },{ "EOR", &a::EOR, &a::IMM, 2 },{ "LSR", &a::LSR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::ABS, 3 },{ "EOR", &a::EOR, &a::ABS, 4 },{ "LSR", &a::LSR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVC", &a::BVC, &a::REL, 2 },{ "EOR", &a::EOR, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ZPX, 4 },{ "LSR", &a::LSR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLI", &a::CLI, &a::IMP, 2 },{ "EOR", &a::EOR, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "EOR", &a::EOR, &a::ABX, 4 },{ "LSR", &a::LSR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "RTS", &a::RTS, &a::IMP, 6 },{ "ADC", &a::ADC, &a::IZX, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 3 },{ "ADC", &a::ADC, &a::ZP0, 3 },{ "ROR", &a::ROR, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "PLA", &a::PLA, &a::IMP, 4 },{ "ADC", &a::ADC, &a::IMM, 2 },{ "ROR", &a::ROR, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "JMP", &a::JMP, &a::IND, 5 },{ "ADC", &a::ADC, &a::ABS, 4 },{ "ROR", &a::ROR, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BVS", &a::BVS, &a::REL, 2 },{ "ADC", &a::ADC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ZPX, 4 },{ "ROR", &a::ROR, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SEI", &a::SEI, &a::IMP, 2 },{ "ADC", &a::ADC, &a::ABY, 4 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "ADC", &a::ADC, &a::ABX, 4 },{ "ROR", &a::ROR, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "???", &a::NOP, &a::IMP, 2 },{ "STA", &a::STA, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZP0, 3 },{ "STA", &a::STA, &a::ZP0, 3 },{ "STX", &a::STX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "DEY", &a::DEY, &a::IMP, 2 },{ "???", &a::NOP, &a::IMP, 2 },{ "TXA", &a::TXA, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "STY", &a::STY, &a::ABS, 4 },{ "STA", &a::STA, &a::ABS, 4 },{ "STX", &a::STX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCC", &a::BCC, &a::REL, 2 },{ "STA", &a::STA, &a::IZY, 6 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "STY", &a::STY, &a::ZPX, 4 },{ "STA", &a::STA, &a::ZPX, 4 },{ "STX", &a::STX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "TYA", &a::TYA, &a::IMP, 2 },{ "STA", &a::STA, &a::ABY, 5 },{ "TXS", &a::TXS, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::NOP, &a::IMP, 5 },{ "STA", &a::STA, &a::ABX, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "???", &a::XXX, &a::IMP, 5 },
		{ "LDY", &a::LDY, &a::IMM, 2 },{ "LDA", &a::LDA, &a::IZX, 6 },{ "LDX", &a::LDX, &a::IMM, 2 },{ "???", &a::XXX, &a::IMP, 6 },{ "LDY", &a::LDY, &a::ZP0, 3 },{ "LDA", &a::LDA, &a::ZP0, 3 },{ "LDX", &a::LDX, &a::ZP0, 3 },{ "???", &a::XXX, &a::IMP, 3 },{ "TAY", &a::TAY, &a::IMP, 2 },{ "LDA", &a::LDA, &a::IMM, 2 },{ "TAX", &a::TAX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "LDY", &a::LDY, &a::ABS, 4 },{ "LDA", &a::LDA, &a::ABS, 4 },{ "LDX", &a::LDX, &a::ABS, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "BCS", &a::BCS, &a::REL, 2 },{ "LDA", &a::LDA, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 5 },{ "LDY", &a::LDY, &a::ZPX, 4 },{ "LDA", &a::LDA, &a::ZPX, 4 },{ "LDX", &a::LDX, &a::ZPY, 4 },{ "???", &a::XXX, &a::IMP, 4 },{ "CLV", &a::CLV, &a::IMP, 2 },{ "LDA", &a::LDA, &a::ABY, 4 },{ "TSX", &a::TSX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 4 },{ "LDY", &a::LDY, &a::ABX, 4 },{ "LDA", &a::LDA, &a::ABX, 4 },{ "LDX", &a::LDX, &a::ABY, 4 },{ "???", &a::XXX, &a::IMP, 4 },
		{ "CPY", &a::CPY, &a::IMM, 2 },{ "CMP", &a::CMP, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPY", &a::CPY, &a::ZP0, 3 },{ "CMP", &a::CMP, &a::ZP0, 3 },{ "DEC", &a::DEC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INY", &a::INY, &a::IMP, 2 },{ "CMP", &a::CMP, &a::IMM, 2 },{ "DEX", &a::DEX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 2 },{ "CPY", &a::CPY, &a::ABS, 4 },{ "CMP", &a::CMP, &a::ABS, 4 },{ "DEC", &a::DEC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BNE", &a::BNE, &a::REL, 2 },{ "CMP", &a::CMP, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ZPX, 4 },{ "DEC", &a::DEC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "CLD", &a::CLD, &a::IMP, 2 },{ "CMP", &a::CMP, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "CMP", &a::CMP, &a::ABX, 4 },{ "DEC", &a::DEC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
		{ "CPX", &a::CPX, &a::IMM, 2 },{ "SBC", &a::SBC, &a::IZX, 6 },{ "???", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "CPX", &a::CPX, &a::ZP0, 3 },{ "SBC", &a::SBC, &a::ZP0, 3 },{ "INC", &a::INC, &a::ZP0, 5 },{ "???", &a::XXX, &a::IMP, 5 },{ "INX", &a::INX, &a::IMP, 2 },{ "SBC", &a::SBC, &a::IMM, 2 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::SBC, &a::IMP, 2 },{ "CPX", &a::CPX, &a::ABS, 4 },{ "SBC", &a::SBC, &a::ABS, 4 },{ "INC", &a::INC, &a::ABS, 6 },{ "???", &a::XXX, &a::IMP, 6 },
		{ "BEQ", &a::BEQ, &a::REL, 2 },{ "SBC", &a::SBC, &a::IZY, 5 },{ "???", &a::XXX, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 8 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ZPX, 4 },{ "INC", &a::INC, &a::ZPX, 6 },{ "???", &a::XXX, &a::IMP, 6 },{ "SED", &a::SED, &a::IMP, 2 },{ "SBC", &a::SBC, &a::ABY, 4 },{ "NOP", &a::NOP, &a::IMP, 2 },{ "???", &a::XXX, &a::IMP, 7 },{ "???", &a::NOP, &a::IMP, 4 },{ "SBC", &a::SBC, &a::ABX, 4 },{ "INC", &a::INC, &a::ABX, 7 },{ "???", &a::XXX, &a::IMP, 7 },
	};

}

//Destructor
CPU::~CPU() {

}

void CPU::write(uint16_t address, uint8_t data) {
	//Calling the bus's write function
	bus->cpu_write(address, data);
}

uint8_t CPU::read(uint16_t address) {
	//Calling the bus's read function, default readonly as false.
	return bus->cpu_read(address, false);
}

/*--------------------------------------------------------------------
	====================== Flag Functions ========================
*-------------------------------------------------------------------*/

// Get the value of a specific bit of the status register
uint8_t CPU::getFlag(CPU_Flags flag)
{
	return ((status & flag) > 0) ? 1 : 0;
}

// Sets a specific bit of the status register
void CPU::setFlag(CPU_Flags flag, bool val)
{
	if (val) { status |= flag; }
	else { status &= ~flag; }
}

/*--------------------------------------------------------------------
	===================== Addressing Modes =======================
*-------------------------------------------------------------------*/

void CPU::clock() {
	//This is not a clock accurate emulation.
	//Remaining cycles is initially set to 0 so we can start the instruction, but depending on the instruction, we may be asked to wait (aka call clock())
	//Multiple times before remaining_cycles is 0 again and we can perform the next instruction.
	if (remaining_cycles == 0) {

		//Grab the opcode - which is stored at the memory address pc.
		opcode = read(pc);

		setFlag(unused, true);

		//Read the next byte - this may need to be adjusted later since the next instruction is not necessarily adjacent on the 6502. 
		pc++;

		//Index the table using the opcode for starting number of cycles.
		remaining_cycles = instructions[opcode].cycles;

		//Call to adjust adressing mode and perform opcode function. Adressing mode and opcode return a value because they may need an additional clock cycle.
		uint8_t additional_cycle_addrmode = (this->*instructions[opcode].address_mode)();
		uint8_t additional_cycle_operate = (this->*instructions[opcode].operate)();

		//Additional cycles are captured and added to remaining_cycles count. This prolongs the duration of the instruction.
		remaining_cycles += (additional_cycle_addrmode & additional_cycle_operate);

		setFlag(unused, true);
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
	address_ABS = pc++;
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
	address_ABS = (high << 8) | low;
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
	address_ABS = (high << 8) | low;
	address_ABS += x;

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
	address_ABS = (high << 8) | low;
	address_ABS += y;

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
	if (!(instructions[opcode].address_mode == &CPU::IMP)) {
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
	setFlag(negative, a & 0x80);
	return 1;
}

//LDX - Load the X register with fetched data.
uint8_t CPU::LDX() {
	fetch();
	x = fetched;
	setFlag(zero, x == 0x00);
	setFlag(negative, x & 0x80);
	return 1;
}

//LDY - Load the Y register with fetched data.
uint8_t CPU::LDY() {
	fetch();
	y = fetched;
	setFlag(zero, y == 0x00);
	setFlag(negative, y & 0x80);
	return 1;
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
	write(0x100 + stkp, a);
	stkp--;
	return 0;
}

//PHA - Push the status register.
uint8_t CPU::PHP() {
	//Set break status to 1 before push.
	write(0x0100 + stkp, status | brk | unused);
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
	status = read(0x0100 + stkp);
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
	return 0;
}

//ADC - Add with carry.
uint8_t CPU::ADC() {
	//Fetch data to add
	fetch();

	//Cast elements to 16 bits.
	temp = (int16_t)a + (uint16_t)fetched + (uint16_t)getFlag(carry);

	//If temp is greater than 255 (max value of an 8-bit integer) we need to set the carry bit.
	setFlag(carry, temp > 255);

	//Set the zero flag - remember we only care about the lower 8 digits since we have cast to a 16-bit integer.
	setFlag(zero, (temp & 0xFF00) == 0);

	//Set overflow according to the folowing logic.
	setFlag(overflow, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);

	//Set negative flag - again we are working with a 16-bit integer.
	setFlag(negative, (temp & 0x80));

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
	uint16_t value = ((uint16_t)fetched) ^ 0x00FF;

	temp = (uint16_t)a + value + (uint16_t)getFlag(carry);
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
	temp = fetched - 1;
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
	temp = fetched + 1;
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
	temp = (uint16_t)fetched << 1;
	setFlag(carry, (temp & 0xFF00) > 0);
	setFlag(zero, (temp & 0x00FF) == 0x00);
	setFlag(negative, temp & 0x80);
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
	temp = (uint16_t)(fetched << 1) | getFlag(carry);
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
	temp = fetched >> 1;
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
	temp = (uint16_t)(getFlag(carry) << 7) | (fetched >> 1);
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
	a = a & fetched;
	//Set zero flag if this operation has zeroed out the accumulator.
	setFlag(zero, a == 0x00);
	//Set negative flag if bit 7 of the accumulator is 1 (aka negative).
	setFlag(negative, a & 0x80);
	//Potentially may require an extra clock cycle if page is crossed, but this is checked in addressing mode. Recall that our clock function will add another
	//cycle if both the instruction and addressing mode call for another cycle.
	return 1;
}

//EOR - Bitwise XOR the accumulator with fetched data.
uint8_t CPU::EOR() {
	fetch();
	//XOR the data in the accumulator with the fetched data.
	a = a ^ fetched;
	//Set zero flag if this operation has zeroed out the accumulator.
	setFlag(zero, a == 0x00);
	//Set negative flag if bit 7 of the accumulator is 1 (aka negative).
	setFlag(negative, a & 0x80);
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
	setFlag(negative, a & 0x80);
	//Potentially may require an extra clock cycle if page is crossed, but this is checked in addressing mode. Recall that our clock function will add another
	//cycle if both the instruction and addressing mode call for another cycle.
	return 1;
}

//CMP - Compare accumulator, sets flags as if a subtraction had been carried out.
uint8_t CPU::CMP() {
	fetch();
	temp = (uint16_t)a - (uint16_t)fetched;
	setFlag(carry, a >= fetched);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 1;
}

//CPX - Compare X register, sets flags as if a subtraction had been carried out.
uint8_t CPU::CPX() {
	fetch();
	temp = (uint16_t)x - (uint16_t)fetched;
	setFlag(carry, x >= fetched);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 0;
}

//CPY - Compare Y register, sets flags as if a subtraction had been carried out.
uint8_t CPU::CPY() {
	fetch();
	temp = (uint16_t)y - (uint16_t)fetched;
	setFlag(carry, y >= fetched);
	setFlag(zero, (temp & 0x00FF) == 0x0000);
	setFlag(negative, temp & 0x0080);
	return 0;
}

//BIT - Bit Test, places bit 7 of the operand into the N flag and bit 6 of the operand into the V flag. 
//The operand is then ANDed with the accumulator, and the Z flag is set if the result is zero.
uint8_t CPU::BIT() {
	fetch();
	temp = a & fetched;
	setFlag(zero, (temp & 0x00FF) == 0x00);
	setFlag(negative, fetched & (1 << 7));
	setFlag(overflow, fetched & (1 << 6));

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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;

		pc = address_ABS;
	}
	return 0;
}

//BCS - Branch if the carry bit of the status register is set.
uint8_t CPU::BCS() {
	//If the carry bit is set
	if (getFlag(carry) == 1) {
		//Add 1 to the remaining cycles
		remaining_cycles++;
		//change address to the relative address from relative addressing, which is initialized as 0
		address_ABS = pc + address_REL;

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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
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

		if ((address_ABS & 0xFF00) != (pc & 0xFF00))
			remaining_cycles++;

		pc = address_ABS;
	}
	return 0;
}

//CLC - Clear carry flag
uint8_t CPU::CLC()
{
	setFlag(carry, false);
	return 0;
}

//CLD - Clear decimal flag
uint8_t CPU::CLD()
{
	setFlag(decimal, false);
	return 0;
}

//CLI - Clear interrupt flag
uint8_t CPU::CLI()
{
	setFlag(disable, false);
	return 0;
}

//CLV - Clear overflow flag
uint8_t CPU::CLV()
{
	setFlag(overflow, false);
	return 0;
}

//SEC - Set carry flag
uint8_t CPU::SEC()
{
	setFlag(carry, true);
	return 0;
}

//SED - Set decimal flag
uint8_t CPU::SED()
{
	setFlag(decimal, true);
	return 0;
}

//SED - Set interrupt disable
uint8_t CPU::SEI()
{
	setFlag(disable, true);
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

//XXX - Captures all illgal opcodes.
uint8_t CPU::XXX()
{
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
	// Get address to set program counter to
	address_ABS = 0xFFFC;
	uint16_t hi = read(address_ABS + 1);
	uint16_t lo = read(address_ABS + 0);

	// Set it
	pc = (hi << 8) | lo;

	// Reset internal registers
	a = 0;
	x = 0;
	y = 0;
	stkp = 0xFD;
	status = 0x00 | unused;

	// Clear internal helper variables
	address_REL = 0x0000;
	address_ABS = 0x0000;
	fetched = 0x00;

	// Reset takes time
	remaining_cycles = 8;
}

void CPU::interrupt() {

	//interrupts are not disabled
	if (getFlag(disable) == 0) {
		//Write the current pc (two writes because the pc is 16 bits)
		write(0x0100 + stkp, (pc >> 8) & 0x00FF);
		stkp--;
		write(0x0100 + stkp, pc & 0x00FF);
		stkp--;

		//Write the status register to the stack
		setFlag(brk, 0);
		setFlag(unused, 1);
		setFlag(disable, 1);
		write(0x0100 + stkp, status);
		stkp--;

		//Default address for pc, where programmers are meant to store code serving the interrupt.
		address_ABS = 0xFFFE;
		uint16_t low = read(address_ABS + 0);
		uint16_t high = read(address_ABS + 1);
		pc = (high << 8) | low;

		remaining_cycles = 7;
	}
}

//Non Maskable interrupt, we dont check the disabled flag
void CPU::non_maskable_interrupt() {
	//Write the current pc (two writes because the pc is 16 bits)
	write(0x0100 + stkp, (pc >> 8) & 0x00FF);
	stkp--;
	write(0x0100 + stkp, pc & 0x00FF);
	stkp--;

	//Write the status register to the stack
	setFlag(brk, 0);
	setFlag(unused, 1);
	setFlag(disable, 1);
	write(0x0100 + stkp, status);
	stkp--;

	//Default address for pc for non maskable interrupts, where programmers are meant to store code serving the interrupt.
	//Note it is not the same address as for regular interrupts.
	address_ABS = 0xFFFA;
	uint16_t low = read(address_ABS + 0);
	uint16_t high = read(address_ABS + 1);
	pc = (high << 8) | low;

	remaining_cycles = 8;
}

bool CPU::complete()
{
	return remaining_cycles == 0;
}

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
	// byte, which in turn yields information from the instructions table
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
		uint8_t _opcode = bus->cpu_read(addr, true); addr++;
		sInst += instructions[_opcode].name + " ";

		// Get oprands from desired locations, and form the
		// instruction based upon its addressing mode. These
		// routines mimmick the actual fetch routine of the
		// 6502 in order to get accurate data as part of the
		// instruction
		if (instructions[_opcode].address_mode == &CPU::IMP)
		{
			sInst += " {IMP}";
		}
		else if (instructions[_opcode].address_mode == &CPU::IMM)
		{
			value = bus->cpu_read(addr, true); addr++;
			sInst += "#$" + hex(value, 2) + " {IMM}";
		}
		else if (instructions[_opcode].address_mode == &CPU::ZP0)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + " {ZP0}";
		}
		else if (instructions[_opcode].address_mode == &CPU::ZPX)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + ", X {ZPX}";
		}
		else if (instructions[_opcode].address_mode == &CPU::ZPY)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = 0x00;
			sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
		}
		else if (instructions[_opcode].address_mode == &CPU::IZX)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = 0x00;
			sInst += "($" + hex(lo, 2) + ", X) {IZX}";
		}
		else if (instructions[_opcode].address_mode == &CPU::IZY)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = 0x00;
			sInst += "($" + hex(lo, 2) + "), Y {IZY}";
		}
		else if (instructions[_opcode].address_mode == &CPU::ABS)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = bus->cpu_read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
		}
		else if (instructions[_opcode].address_mode == &CPU::ABX)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = bus->cpu_read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
		}
		else if (instructions[_opcode].address_mode == &CPU::ABY)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = bus->cpu_read(addr, true); addr++;
			sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
		}
		else if (instructions[_opcode].address_mode == &CPU::IND)
		{
			lo = bus->cpu_read(addr, true); addr++;
			hi = bus->cpu_read(addr, true); addr++;
			sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
		}
		else if (instructions[_opcode].address_mode == &CPU::REL)
		{
			value = bus->cpu_read(addr, true); addr++;
			sInst += "$" + hex(value, 2) + " [$" + hex(addr + (int8_t)value, 4) + "] {REL}";
		}

		// Add the formed string to a std::map, using the instruction's
		// address as the key. This makes it convenient to look for later
		// as the instructions are variable in length, so a straight up
		// incremental index is not sufficient.
		mapLines[line_addr] = sInst;
	}

	return mapLines;
}