// Fill out your copyright notice in the Description page of Project Settings.

#pragma once

#include "CoreMinimal.h"
#include <iostream>
#include <fstream>
#include <cstdint>
#include <vector>
#include <memory>
#include "mapper_0.h"

/**
 *
 */
class THE_INDOOR_API Cartridge
{
public:
    // --------- Cartridge constructor/destructor ---------

        //Constructor
    Cartridge();

    //Destructor
    ~Cartridge();

    void initialize(const std::string& filename);

    // ---------- Communications with Main Bus -------------

    bool cpu_read(uint16_t address, uint8_t& data);
    bool cpu_write(uint16_t address, uint8_t data);

    // ---------- Communications with PPU Bus --------------

    bool ppu_read(uint16_t address, uint8_t& data);
    bool ppu_write(uint16_t address, uint8_t data);

    bool ImageValid();

    enum MIRROR
    {
        HORIZONTAL,
        VERTICAL,
        ONESCREEN_LO,
        ONESCREEN_HI,
    } mirror = HORIZONTAL;

    void reset();

private:
    // ----------------- Private Variables ------------------

        //Program memory of the Cartridge
    std::vector<uint8_t> prg_memory;

    //Character memory of the Cartridge
    std::vector<uint8_t> chr_memory;

    //Number of banks and mapper circuits will vary from game to game
    uint8_t mapperid = 0;
    uint8_t prgbanks = 0;
    uint8_t chrbanks = 0;

    //Mapper
    std::shared_ptr<Mapper> mapper_ptr;

    bool bImageValid = false;
};


