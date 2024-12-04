// Fill out your copyright notice in the Description page of Project Settings.

#include "mapper_0.h"
#include <iostream>

Mapper_0::Mapper_0(uint8_t prgBanks, uint8_t chrBanks) : Mapper(prgBanks, chrBanks)
{
}

Mapper_0::~Mapper_0() {

}

void Mapper_0::reset()
{

}

bool Mapper_0::cpu_map_read(uint16_t address, uint32_t& mapped_address)
{
	if (address >= 0x8000 && address <= 0xFFFF) {
		mapped_address = address & (prgbanks > 1 ? 0x7FFF : 0x3FFF);
		return true;
	}

	return false;
}

bool Mapper_0::cpu_map_write(uint16_t address, uint32_t& mapped_address, uint8_t data)
{
	if (address >= 0x8000 && address <= 0xFFFF) {
		mapped_address = address & (prgbanks > 1 ? 0x7FFF : 0x3FFF);
		return true;
	}

	return false;
}

bool Mapper_0::ppu_map_read(uint16_t address, uint32_t& mapped_address)
{
	if (address >= 0x0000 && address <= 0x1FFF) {
		mapped_address = address;
		return true;
	}

	return false;
}

bool Mapper_0::ppu_map_write(uint16_t address, uint32_t& mapped_address)
{
	if (address >= 0x0000 && address <= 0x1FFF) {
		if (chrbanks == 0)
		{
			mapped_address = address;
			return true;
		}
	}

	return false;
}

