// Fill out your copyright notice in the Description page of Project Settings.

#include "cartridge.h"

Cartridge::Cartridge() {

}

Cartridge::~Cartridge()
{
}

void Cartridge::initialize(const std::string& filename) {
	//Header for NES ROMS, iNES formatting
	struct sHeader
	{
		char name[4];
		uint8_t prg_rom_chunks;
		uint8_t chr_rom_chunks;
		uint8_t map1;
		uint8_t map2;
		uint8_t prg_ram_size;
		uint8_t tv1;
		uint8_t tv2;
		char unused[5];
	} header;

	bImageValid = false;

	std::ifstream ifs;
	ifs.open(filename, std::ifstream::binary);
	if (ifs.is_open())
	{
		// Read file header
		ifs.read((char*)&header, sizeof(sHeader));

		// Skip trainer
		if (header.map1 & 0x04)
			ifs.seekg(512, std::ios_base::cur);

		// Get Mapper ID
		mapperid = ((header.map2 >> 4) << 4) | (header.map1 >> 4);
		mirror = (header.map1 & 0x01) ? VERTICAL : HORIZONTAL;

		// Set file format
		uint8_t nFileType = 1;

		if (nFileType == 0)
		{

		}

		if (nFileType == 1)
		{
			prgbanks = header.prg_rom_chunks;
			prg_memory.resize(prgbanks * 16384);
			ifs.read((char*)prg_memory.data(), prg_memory.size());

			chrbanks = header.chr_rom_chunks;
			if (chrbanks == 0)
			{
				chr_memory.resize(8192);
			}
			else
			{
				chr_memory.resize(chrbanks * 8192);
			}
			ifs.read((char*)chr_memory.data(), chr_memory.size());
		}

		if (nFileType == 2)
		{

		}

		// Load appropriate mapper - this emu only supports mapper 0 atm
		switch (mapperid)
		{
		case 0:
			UE_LOG(LogTemp, Log, TEXT("Making mapper to rom."));
			mapper_ptr = std::make_shared<Mapper_0>(prgbanks, chrbanks);
			break;
		}

		bImageValid = true;
		ifs.close();
	}
}

bool Cartridge::ImageValid()
{
	return bImageValid;
}

void Cartridge::reset()
{
	if (mapper_ptr != nullptr)
		mapper_ptr->reset();
}

bool Cartridge::cpu_read(uint16_t address, uint8_t& data)
{
	if (mapper_ptr == nullptr) {
		return false;
	}
	uint32_t mapped_address = 0;
	if (mapper_ptr->cpu_map_read(address, mapped_address)) {
		data = prg_memory[mapped_address];
		return true;
	}

	return false;
}

bool Cartridge::cpu_write(uint16_t address, uint8_t data)
{
	uint32_t mapped_address = 0;
	if (mapper_ptr->cpu_map_write(address, mapped_address, data)) {
		prg_memory[mapped_address] = data;
		return true;
	}
	return false;
}

bool Cartridge::ppu_read(uint16_t address, uint8_t& data)
{
	uint32_t mapped_address = 0;
	if (mapper_ptr->ppu_map_read(address, mapped_address))
	{
		data = chr_memory[mapped_address];
		return true;
	}

	return false;
}

bool Cartridge::ppu_write(uint16_t address, uint8_t data)
{
	uint32_t mapped_address = 0;
	if (mapper_ptr->ppu_map_write(address, mapped_address)) {
		chr_memory[mapped_address] = data;
		return true;
	}
	return false;
}
