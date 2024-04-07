#pragma once
#include <cstdint>
#include "cpu.h"
#include <array>

class Bus {

    public:
    // ------------ Bus constructor/destructor -------------

        //Constructor
        Bus();
        
        //Destructor
        ~Bus();

    // ---------------- Bus read/write ---------------------

        //Write 8 bits of data to a 16 bit address in memory.
        void write(uint16_t address, uint8_t data);

        //Read 8 bits of data from a 16 bit address in memory.
        uint8_t read(uint16_t address, bool readonly);

    // ------------------ Bus devices ----------------------
    
        //Our 6502 CPU.
        CPU cpu;

        //Temporary fake RAM.
        std::array<uint8_t, 64 * 1024> ram;

};