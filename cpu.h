#pragma once
#include <iostream>

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
        uint8_t stpk = 0x00;
        //Program Counter
        uint16_t pc = 0x00;
        
    // ---------------- CPU Adressing Modes -----------------

        uint8_t IMP();  uint8_t IMM();

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

        //Memory address of our bus.
        Bus* bus = nullptr;

        //Functions to access the status register.
        uint8_t getFlag(CPU_Flags flag);
        void setFlad(CPU_Flags flag, bool value);

};