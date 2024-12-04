// Fill out your copyright notice in the Description page of Project Settings.

#pragma once

#include "mapper.h"

#include "CoreMinimal.h"

/**
 *
 */
class THE_INDOOR_API Mapper_0 : public Mapper
{
public:
	// --------- Mapper_0 constructor/destructor ---------

		//Constructor
	Mapper_0(uint8_t pbanks, uint8_t cbanks);
	//Destructor
	~Mapper_0();

	// -------------- Virtual Read/Write ---------------

	bool cpu_map_read(uint16_t address, uint32_t& mapped_address) override;
	bool cpu_map_write(uint16_t address, uint32_t& mapped_address, uint8_t data = 0) override;

	bool ppu_map_read(uint16_t address, uint32_t& mapped_address) override;
	bool ppu_map_write(uint16_t address, uint32_t& mapped_address) override;

	void reset() override;

};