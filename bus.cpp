 #include "bus.h"
#include <iostream>

//Constructor
Bus::Bus() {
    //Clear out our RAM.
    for (auto &data : ram) data = 0x00; 

    //Connect the CPU to our bus.
    cpu.connectBus(this);
}

//Destructor
Bus::~Bus() {

}

void Bus::write(uint16_t address, uint8_t data) {

    //Check to make sure we are writing to a valid memory location.
    if (address >= 0x0000 && address <= 0xFFFF) {
        //If valid, write our data at the address in RAM.
        ram[address] = data;
        return;
    }

    //Specific error message
    std::cout << "Invalid memory address (writing data).\n";
}

uint8_t Bus::read(uint16_t address, bool readonly) {

    //Check to make sure we are reading from a valid memory location.
    if (address >= 0x0000 && address <= 0xFFFF) {
        //If valid, return the data at the address in RAM.
        return ram[address];
    }

    //Specific error message
    std::cout << "Invalid memory address (reading data).\n";
    return 0x00;
}