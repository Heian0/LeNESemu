// Fill out your copyright notice in the Description page of Project Settings.

#pragma once

#include <cstdint>
#include "CoreMinimal.h"

/**
 *
 */
class THE_INDOOR_API Mapper
{
public:
	// --------- Mapper constructor/destructor ---------

		//Constructor
	Mapper(uint8_t pbanks, uint8_t cbanks);
	//Destructor
	~Mapper();

	// -------------- Virtual Read/Write ---------------

		// Map CPU bus address into prgrom location
	virtual bool cpu_map_read(uint16_t address, uint32_t& mapped_address) = 0;
	virtual bool cpu_map_write(uint16_t address, uint32_t& mapped_address, uint8_t data = 0) = 0;

	// Map PPU bus address into chrrom location
	virtual bool ppu_map_read(uint16_t address, uint32_t& mapped_address) = 0;
	virtual bool ppu_map_write(uint16_t address, uint32_t& mapped_address) = 0;

	virtual void reset() = 0;

	// Derived wrappers can access these
protected:
	uint8_t prgbanks;
	uint8_t chrbanks;
};