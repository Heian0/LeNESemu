// Fill out your copyright notice in the Description page of Project Settings.

#pragma once

#include <cstdint>
#include <array>
#include "cpu.h"
#include "ppu.h"
#include "cartridge.h"
#include "CoreMinimal.h"

/**
 *
 */
class THE_INDOOR_API Bus
{
public:

    //Constructor
    Bus();

    //Destructor
    ~Bus();

    // ---------------- Bus read/write ---------------------

        //Write 8 bits of data to a 16 bit address in memory.
    void cpu_write(uint16_t address, uint8_t data);

    //Read 8 bits of data from a 16 bit address in memory.
    uint8_t cpu_read(uint16_t address, bool readonly = false);

    // ------------------ Bus devices ----------------------

    //Our 6502 CPU.
    CPU cpu;

    //The NES PPU.
    PPU ppu;

    uint8_t cpu_ram[2048] = {};

    //Cartridge
    std::shared_ptr<Cartridge> cartridge;

    //Controller
    uint8_t controller[2];

    // --------------- System Interfacing -------------------

        //Insert a Cartridge/ROM into memory
    void insert_cartridge(const std::shared_ptr<Cartridge>& cartridge);

    //Reset the NES
    void reset();

    //Perform a single system tick
    void clock();

private:
    // ----------------- Private Variables ------------------

        //Count of how many clock ticks have passed in total
    uint32_t system_clock_count = 0;

    // For direct memory access
    uint8_t dma_page = 0x00;
    uint8_t dma_addr = 0x00;
    uint8_t dma_data = 0x00;
    bool dma_temp = true;
    bool dma_transfer = false;

    // Internal controller state
    uint8_t controller_state[2];

};
