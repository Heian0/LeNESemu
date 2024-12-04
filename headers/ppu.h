// Fill out your copyright notice in the Description page of Project Settings.

#pragma once

#include "cartridge.h"
#include <cstdint>
#include <memory>
#include "CoreMinimal.h"

class THE_INDOOR_API PPU
{
public:
	PPU();
	~PPU();

private:
	//Our screen object for unreal
	TArray<FColor> screen;
	TArray<FColor> nametable[2];
	TArray<FColor> patterntable[2];

	uint8_t     tblName[2][1024];
	uint8_t     tblPattern[2][4096];
	uint8_t 	tblPalette[32];

	FColor palScreen[0x40];
	//Width of our rendered display, for use in setting pixels in the screen
	int screen_width = 256;
	//Height of our rendered display, for use in setting pixels in the screen
	int screen_height = 240;

	int nametable_width = 256;
	int nametable_height = 240;

	int patterntable_width = 128, patterntable_height = 128;

	FColor black_pixel = FColor(0, 0, 0, 255);

	// The Cartridge or "GamePak"
	std::shared_ptr<Cartridge> cart;



public:
	void set_pixel(TArray<FColor>& screen, int r, int c, FColor pixel, int id);
	// Debugging Utilities
	TArray<FColor>& get_screen();
	//olc::Sprite& GetScreen();
	TArray<FColor>& get_nametable(uint8_t i);
	//olc::Sprite& GetNameTable(uint8_t i);
	TArray<FColor>& get_patterntable(uint8_t i, uint8_t palette);
	//olc::Sprite& GetPatternTable(uint8_t i, uint8_t palette);
	FColor& get_colour_from_palette_ram(uint8_t palette, uint8_t pixel);
	//olc::Pixel& get_colour_from_palette_ram(uint8_t palette, uint8_t pixel);

	bool frame_complete = false;

private:

	union
	{
		struct
		{
			uint8_t unused : 5;
			uint8_t sprite_overflow : 1;
			uint8_t sprite_zero_hit : 1;
			uint8_t vertical_blank : 1;
		};

		uint8_t reg;
	} status;


	union
	{
		struct
		{
			uint8_t grayscale : 1;
			uint8_t render_background_left : 1;
			uint8_t render_sprites_left : 1;
			uint8_t render_background : 1;
			uint8_t render_sprites : 1;
			uint8_t enhance_red : 1;
			uint8_t enhance_green : 1;
			uint8_t enhance_blue : 1;
		};

		uint8_t reg;
	} mask;

	union PPUCTRL
	{
		struct
		{
			uint8_t nametable_x : 1;
			uint8_t nametable_y : 1;
			uint8_t increment_mode : 1;
			uint8_t pattern_sprite : 1;
			uint8_t pattern_background : 1;
			uint8_t sprite_size : 1;
			uint8_t slave_mode : 1; // unused
			uint8_t enable_nmi : 1;
		};

		uint8_t reg;
	} control;

	union loopy_register
	{
		struct
		{

			uint16_t coarse_x : 5;
			uint16_t coarse_y : 5;
			uint16_t nametable_x : 1;
			uint16_t nametable_y : 1;
			uint16_t fine_y : 3;
			uint16_t unused : 1;
		};

		uint16_t reg = 0x0000;
	};


	loopy_register vram_addr;
	loopy_register tram_addr;

	uint8_t fine_x = 0x00;

	// Internal communications
	uint8_t address_latch = 0x00;
	uint8_t ppu_data_buffer = 0x00;

	int16_t scanline = 0;
	int16_t cycle = 0;

	uint8_t bg_next_tile_id = 0x00;
	uint8_t bg_next_tile_attrib = 0x00;
	uint8_t bg_next_tile_lsb = 0x00;
	uint8_t bg_next_tile_msb = 0x00;
	uint16_t bg_shifter_pattern_lo = 0x0000;
	uint16_t bg_shifter_pattern_hi = 0x0000;
	uint16_t bg_shifter_attrib_lo = 0x0000;
	uint16_t bg_shifter_attrib_hi = 0x0000;

	struct sObjectAttributeEntry
	{
		uint8_t y;
		uint8_t id;
		uint8_t attribute;
		uint8_t x;
	} OAM[64];

	uint8_t oam_addr = 0x00;

	sObjectAttributeEntry spriteScanline[8];
	uint8_t sprite_count;
	uint8_t sprite_shifter_pattern_lo[8];
	uint8_t sprite_shifter_pattern_hi[8];

	// Sprite Zero Collision Flags
	bool bSpriteZeroHitPossible = false;
	bool bSpriteZeroBeingRendered = false;

public:
	uint8_t* pOAM = (uint8_t*)OAM;

	// Communications with Main Bus
	uint8_t cpu_read(uint16_t addr, bool rdonly = false);
	void    cpu_write(uint16_t addr, uint8_t  data);

	// Communications with PPU Bus
	uint8_t ppu_read(uint16_t addr, bool rdonly = false);
	void    ppu_write(uint16_t addr, uint8_t data);
	// Interface
	void connect_cartridge(const std::shared_ptr<Cartridge>& cartridge);
	void clock();
	void reset();
	bool nmi = false;
};


