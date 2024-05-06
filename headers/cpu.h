#pragma once
#include <iostream>
#include <vector>


class Bus;

class CPU {

    public:
    // ------------ CPU constructor/destructor -------------

        //Constructor
        CPU();
        
        //Destructor
        ~CPU();

    // ------------- CPU status register bits --------------

        enum CPU_Flags {
            //Carry bit
            carry = (1 << 0),
            //Zero
            zero = (1 << 1),
            //Disable interrupts
            disable = (1 << 2),
            //Decimal mode
            decimal = (1 << 3),
            //Break
            brk = (1 << 4),
            //Unused
            unused = (1 << 5),
            //Overflow
            overflow = (1 << 6),
            //Negative
            negative = (1 << 7)
        };

    // ------------------ CPU Registers --------------------

        //Status Register
        uint8_t status = 0x00;
        //Accumulator Register
        uint8_t a = 0x00;
        //X Register
        uint8_t x = 0x00;
        //Y Register
        uint8_t y = 0x00;
        //Stack Pointer - Points to a location on the bus
        uint8_t stkp = 0x00;
        //Program Counter
        uint16_t pc = 0x0000;
        
    // ---------------- CPU Adressing Modes -----------------

        uint8_t IMP();  uint8_t IMM();
        uint8_t ZP0();  uint8_t ZPX();
        uint8_t ZPY();  uint8_t REL();
        uint8_t ABS();  uint8_t ABX();
        uint8_t ABY();  uint8_t IND();
        uint8_t IZX();  uint8_t IZY();

    // ------------------ CPU Opcodes ----------------------

        //Legal Opcodes
        uint8_t ADC();	uint8_t AND();	uint8_t ASL();	uint8_t BCC();
        uint8_t BCS();	uint8_t BEQ();	uint8_t BIT();	uint8_t BMI();
        uint8_t BNE();	uint8_t BPL();	uint8_t BRK();	uint8_t BVC();
        uint8_t BVS();	uint8_t CLC();	uint8_t CLD();	uint8_t CLI();
        uint8_t CLV();	uint8_t CMP();	uint8_t CPX();	uint8_t CPY();
        uint8_t DEC();	uint8_t DEX();	uint8_t DEY();	uint8_t EOR();
        uint8_t INC();	uint8_t INX();	uint8_t INY();	uint8_t JMP();
        uint8_t JSR();	uint8_t LDA();	uint8_t LDX();	uint8_t LDY();
        uint8_t LSR();	uint8_t NOP();	uint8_t ORA();	uint8_t PHA();
        uint8_t PHP();	uint8_t PLA();	uint8_t PLP();	uint8_t ROL();
        uint8_t ROR();	uint8_t RTI();	uint8_t RTS();	uint8_t SBC();
        uint8_t SEC();	uint8_t SED();	uint8_t SEI();	uint8_t STA();
        uint8_t STX();	uint8_t STY();	uint8_t TAX();	uint8_t TAY();
        uint8_t TSX();	uint8_t TXA();	uint8_t TXS();	uint8_t TYA();

        //Illegal Opcodes
        uint8_t XXX();

    // ------------------- CPU Signals ---------------------

        //Clock Signal - Indicate to the CPU one clock cycle has occured.
        void clock();

        //The following 3 signals stop what the CPU is doing - although it will finish its current instruction.

        //Reset
        void reset();
        //System Interrupt - Can be ignored if our status register has ignore on.
        void interrupt();
        //Non Maskable System Interrupt - Can never be ignored.
        void non_maskable_interrupt();
    
    // ------------ Disassembler (Temporary) ---------------

        std::map<uint16_t, std::string> disassemble(uint16_t nStart, uint16_t nStop);

        bool complete();

    // ----------------- CPU Helper Functions --------------

        //Fetch Data
        uint8_t fetch();
        //Fetched Data
        uint8_t fetched = 0x00;
        //Address for directly reading from memory
        uint16_t address_ABS = 0x0000;
        //Relative address for branching
        uint16_t address_REL = 0x0000;
        //Current Opcode
        uint8_t opcode = 0x00;
        //Cycles remaining for this instruction.
        uint8_t remaining_cycles = 0;

    // --------------- CPU -> Bus connection ---------------

        //Connect the bus to our CPU by assigning private member bus_ptr the memory location of our bus.
        void connectBus(Bus* ptr) {
            bus = ptr;
        }

    // ---------------- CPU read/write ---------------------

        //Write 8 bits of data to a 16 bit address in memory.
        void write(uint16_t address, uint8_t data);

        //Read 8 bits of data from a 16 bit address in memory.
        uint8_t read(uint16_t address);

    private:

        struct instruction {
            //Name of instruction
            std::string name;
            //Points to an addressing mode function
            uint8_t(CPU::*address_mode)(void) = nullptr;
            //Points to an operation function
            uint8_t(CPU::*operate)(void) = nullptr;
            //Cycles required to excecute this operation
            uint8_t cycles;
        };

        //Memory address of our bus.
        Bus* bus = nullptr;

        //Functions to access the status register.
        uint8_t getFlag(CPU_Flags flag);
        void setFlag(CPU_Flags flag, bool value);

        std::vector<instruction> instructions;
};