// Fill out your copyright notice in the Description page of Project Settings.


#include "Bus.h"

//Constructor
Bus::Bus() {
	//Connect the CPU to our bus.
	cpu.connectBus(this);
}

//Destructor
Bus::~Bus() {

}

void Bus::cpu_write(uint16_t address, uint8_t data) {

	// If we are writing to the cartridge (aka cpu_write returns true) we dont change the ram/ppu
	if (cartridge->cpu_write(address, data)) {}

	//Check to make sure we are writing to a valid memory location.
	else if (address >= 0x0000 && address <= 0x1FFF) {
		//If valid, write our data at the address in RAM.
		cpu_ram[address & 0x07FF] = data;
	}

	//Writing to the PPU
	else if (address >= 0x2000 && address <= 0x3FFF) {
		ppu.cpu_write(address & 0x0007, data);
	}
	// A write to this address initiates a DMA transfer
	else if (address == 0x4014)
	{
		dma_page = data;
		dma_addr = 0x00;
		dma_transfer = true;
	}

	// Lock controller state
	else if (address >= 0x4016 && address <= 0x4017)
	{

		controller_state[address & 0x0001] = controller[address & 0x0001];
	}
}

uint8_t Bus::cpu_read(uint16_t address, bool readonly) {


	uint8_t data = 0x00;

	if (cartridge->cpu_read(address, data)) {}

	//Check to make sure we are reading from a valid memory location.
	else if (address >= 0x0000 && address <= 0x1FFF) {
		return cpu_ram[address & 0x07FF];
	}

	else if (address >= 0x2000 && address <= 0x3FFF) {
		data = ppu.cpu_read(address & 0x0007, readonly);
	}

	else if (address >= 0x4016 && address <= 0x4017)
	{
		data = (controller_state[address & 0x0001] & 0x80) > 0;
		controller_state[address & 0x0001] <<= 1;
	}


	return data;

}

void Bus::insert_cartridge(const std::shared_ptr<Cartridge>& cart)
{
	this->cartridge = cart;
	ppu.connect_cartridge(cart);
}

void Bus::reset()
{
	cartridge->reset();
	cpu.reset();
	ppu.reset();
	system_clock_count = 0;
	dma_page = 0x00;
	dma_addr = 0x00;
	dma_data = 0x00;
	dma_temp = true;
	dma_transfer = false;
}


void Bus::clock()
{
	ppu.clock();
	// CPU executes once for every 3 PPU instructions
	if (system_clock_count % 3 == 0) {
		if (dma_transfer)
		{
			if (dma_temp)
			{
				if (system_clock_count % 2 == 1)
				{
					dma_temp = false;
				}
			}
			else
			{
				if (system_clock_count % 2 == 0)
				{
					dma_data = cpu_read(dma_page << 8 | dma_addr);
				}
				else
				{
					ppu.pOAM[dma_addr] = dma_data;
					dma_addr++;
					if (dma_addr == 0x00)
					{
						dma_transfer = false;
						dma_temp = true;
					}
				}
			}
		}
		else
		{
			cpu.clock();
		}
	}

	if (ppu.nmi)
	{
		ppu.nmi = false;
		cpu.non_maskable_interrupt();
	}

	system_clock_count++;
}
