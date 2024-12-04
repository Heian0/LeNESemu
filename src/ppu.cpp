// Fill out your copyright notice in the Description page of Project Settings.


#include "ppu.h"

PPU::PPU()
{
	palScreen[0x00] = FColor(84, 84, 84);
	palScreen[0x01] = FColor(0, 30, 116);
	palScreen[0x02] = FColor(8, 16, 144);
	palScreen[0x03] = FColor(48, 0, 136);
	palScreen[0x04] = FColor(68, 0, 100);
	palScreen[0x05] = FColor(92, 0, 48);
	palScreen[0x06] = FColor(84, 4, 0);
	palScreen[0x07] = FColor(60, 24, 0);
	palScreen[0x08] = FColor(32, 42, 0);
	palScreen[0x09] = FColor(8, 58, 0);
	palScreen[0x0A] = FColor(0, 64, 0);
	palScreen[0x0B] = FColor(0, 60, 0);
	palScreen[0x0C] = FColor(0, 50, 60);
	palScreen[0x0D] = FColor(0, 0, 0);
	palScreen[0x0E] = FColor(0, 0, 0);
	palScreen[0x0F] = FColor(0, 0, 0);

	palScreen[0x10] = FColor(152, 150, 152);
	palScreen[0x11] = FColor(8, 76, 196);
	palScreen[0x12] = FColor(48, 50, 236);
	palScreen[0x13] = FColor(92, 30, 228);
	palScreen[0x14] = FColor(136, 20, 176);
	palScreen[0x15] = FColor(160, 20, 100);
	palScreen[0x16] = FColor(152, 34, 32);
	palScreen[0x17] = FColor(120, 60, 0);
	palScreen[0x18] = FColor(84, 90, 0);
	palScreen[0x19] = FColor(40, 114, 0);
	palScreen[0x1A] = FColor(8, 124, 0);
	palScreen[0x1B] = FColor(0, 118, 40);
	palScreen[0x1C] = FColor(0, 102, 120);
	palScreen[0x1D] = FColor(0, 0, 0);
	palScreen[0x1E] = FColor(0, 0, 0);
	palScreen[0x1F] = FColor(0, 0, 0);

	palScreen[0x20] = FColor(236, 238, 236);
	palScreen[0x21] = FColor(76, 154, 236);
	palScreen[0x22] = FColor(120, 124, 236);
	palScreen[0x23] = FColor(176, 98, 236);
	palScreen[0x24] = FColor(228, 84, 236);
	palScreen[0x25] = FColor(236, 88, 180);
	palScreen[0x26] = FColor(236, 106, 100);
	palScreen[0x27] = FColor(212, 136, 32);
	palScreen[0x28] = FColor(160, 170, 0);
	palScreen[0x29] = FColor(116, 196, 0);
	palScreen[0x2A] = FColor(76, 208, 32);
	palScreen[0x2B] = FColor(56, 204, 108);
	palScreen[0x2C] = FColor(56, 180, 204);
	palScreen[0x2D] = FColor(60, 60, 60);
	palScreen[0x2E] = FColor(0, 0, 0);
	palScreen[0x2F] = FColor(0, 0, 0);

	palScreen[0x30] = FColor(236, 238, 236);
	palScreen[0x31] = FColor(168, 204, 236);
	palScreen[0x32] = FColor(188, 188, 236);
	palScreen[0x33] = FColor(212, 178, 236);
	palScreen[0x34] = FColor(236, 174, 236);
	palScreen[0x35] = FColor(236, 174, 212);
	palScreen[0x36] = FColor(236, 180, 176);
	palScreen[0x37] = FColor(228, 196, 144);
	palScreen[0x38] = FColor(204, 210, 120);
	palScreen[0x39] = FColor(180, 222, 120);
	palScreen[0x3A] = FColor(168, 226, 144);
	palScreen[0x3B] = FColor(152, 226, 180);
	palScreen[0x3C] = FColor(160, 214, 228);
	palScreen[0x3D] = FColor(160, 162, 160);
	palScreen[0x3E] = FColor(0, 0, 0);
	palScreen[0x3F] = FColor(0, 0, 0);
	screen.Init(black_pixel, 256 * 240);
	//sprScreen = new olc::Sprite(256, 240);
	nametable[0].Init(black_pixel, 256 * 240);
	//sprNameTable[0] = new olc::Sprite(256, 240);
	nametable[1].Init(black_pixel, 256 * 240);
	//sprNameTable[1] = new olc::Sprite(256, 240);
	patterntable[0].Init(black_pixel, 128 * 128);
	//sprPatternTable[0] = new olc::Sprite(128, 128);
	patterntable[1].Init(black_pixel, 128 * 128);
	//sprPatternTable[1] = new olc::Sprite(128, 128);
}


PPU::~PPU()
{
	//delete sprScreen;
	//delete sprNameTable[0];
	//delete sprNameTable[1];
	//delete sprPatternTable[0];
	//delete sprPatternTable[1];
}

FColor& PPU::get_colour_from_palette_ram(uint8_t palette, uint8_t pixel)
{
	return palScreen[ppu_read(0x3F00 + (palette << 2) + pixel) & 0x3F];
}

TArray<FColor>& PPU::get_nametable(uint8_t i)
{
	return nametable[i];
}

TArray<FColor>& PPU::get_patterntable(uint8_t i, uint8_t palette)
{
	for (uint16_t nTileY = 0; nTileY < 16; nTileY++)
	{
		for (uint16_t nTileX = 0; nTileX < 16; nTileX++)
		{
			uint16_t nOffset = nTileY * 256 + nTileX * 16;

			for (uint16_t row = 0; row < 8; row++)
			{
				uint8_t tile_lsb = ppu_read(i * 0x1000 + nOffset + row + 0x0000);
				uint8_t tile_msb = ppu_read(i * 0x1000 + nOffset + row + 0x0008);

				for (uint16_t col = 0; col < 8; col++)
				{
					uint8_t pixel = (tile_lsb & 0x01) << 1 | (tile_msb & 0x01);
					tile_lsb >>= 1; tile_msb >>= 1;

					set_pixel(patterntable[i], nTileX * 8 + (7 - col), nTileY * 8 + row, get_colour_from_palette_ram(palette, pixel), 2);
				}
			}
		}
	}

	return patterntable[i];
}

uint8_t PPU::cpu_read(uint16_t addr, bool rdonly)
{
	uint8_t data = 0x00;

	if (rdonly)
	{

		switch (addr)
		{
		case 0x0000: // Control
			data = control.reg;
			break;
		case 0x0001: // Mask
			data = mask.reg;
			break;
		case 0x0002: // Status
			data = status.reg;
			break;
		case 0x0003: // OAM Address
			break;
		case 0x0004: // OAM Data
			break;
		case 0x0005: // Scroll
			break;
		case 0x0006: // PPU Address
			break;
		case 0x0007: // PPU Data
			break;
		}
	}
	else
	{
		switch (addr)
		{
			// Control - Not readable
		case 0x0000: break;

			// Mask - Not Readable
		case 0x0001: break;

			// Status
		case 0x0002:

			data = (status.reg & 0xE0) | (ppu_data_buffer & 0x1F);

			// Clear the vertical blanking flag
			status.vertical_blank = 0;

			// Reset Loopy's Address latch flag
			address_latch = 0;
			break;

			// OAM Address - Not Readable
		case 0x0003: break;

			// OAM Data
		case 0x0004:
			data = pOAM[oam_addr];
			break;

			// Scroll - Not Readable
		case 0x0005: break;

			// PPU Address - Not Readable
		case 0x0006: break;

			// PPU Data
		case 0x0007:
			data = ppu_data_buffer;
			ppu_data_buffer = ppu_read(vram_addr.reg);
			if (vram_addr.reg >= 0x3F00) data = ppu_data_buffer;
			vram_addr.reg += (control.increment_mode ? 32 : 1);
			break;
		}
	}

	return data;
}

void PPU::cpu_write(uint16_t addr, uint8_t data)
{
	switch (addr)
	{
	case 0x0000: // Control
		control.reg = data;
		tram_addr.nametable_x = control.nametable_x;
		tram_addr.nametable_y = control.nametable_y;
		break;
	case 0x0001: // Mask
		mask.reg = data;
		break;
	case 0x0002: // Status
		break;
	case 0x0003: // OAM Address
		oam_addr = data;
		break;
	case 0x0004: // OAM Data
		pOAM[oam_addr] = data;
		break;
	case 0x0005: // Scroll
		if (address_latch == 0)
		{
			// First write to scroll register contains X offset in pixel space
			// which we split into coarse and fine x values
			fine_x = data & 0x07;
			tram_addr.coarse_x = data >> 3;
			address_latch = 1;
		}
		else
		{
			// First write to scroll register contains Y offset in pixel space
			// which we split into coarse and fine Y values
			tram_addr.fine_y = data & 0x07;
			tram_addr.coarse_y = data >> 3;
			address_latch = 0;
		}
		break;
	case 0x0006: // PPU Address
		if (address_latch == 0)
		{

			tram_addr.reg = (uint16_t)((data & 0x3F) << 8) | (tram_addr.reg & 0x00FF);
			address_latch = 1;
		}
		else
		{
			tram_addr.reg = (tram_addr.reg & 0xFF00) | data;
			vram_addr = tram_addr;
			address_latch = 0;
		}
		break;
	case 0x0007: // PPU Data
		ppu_write(vram_addr.reg, data);
		vram_addr.reg += (control.increment_mode ? 32 : 1);
		break;
	}
}

uint8_t PPU::ppu_read(uint16_t addr, bool rdonly)
{
	uint8_t data = 0x00;
	addr &= 0x3FFF;

	if (cart->ppu_read(addr, data))
	{

	}
	else if (addr >= 0x0000 && addr <= 0x1FFF)
	{
		// If the cartridge cant map the address, have
		// a physical location ready here
		data = tblPattern[(addr & 0x1000) >> 12][addr & 0x0FFF];
	}
	else if (addr >= 0x2000 && addr <= 0x3EFF)
	{
		addr &= 0x0FFF;

		if (cart->mirror == Cartridge::MIRROR::VERTICAL)
		{
			// Vertical
			if (addr >= 0x0000 && addr <= 0x03FF)
				data = tblName[0][addr & 0x03FF];
			if (addr >= 0x0400 && addr <= 0x07FF)
				data = tblName[1][addr & 0x03FF];
			if (addr >= 0x0800 && addr <= 0x0BFF)
				data = tblName[0][addr & 0x03FF];
			if (addr >= 0x0C00 && addr <= 0x0FFF)
				data = tblName[1][addr & 0x03FF];
		}
		else if (cart->mirror == Cartridge::MIRROR::HORIZONTAL)
		{
			// Horizontal
			if (addr >= 0x0000 && addr <= 0x03FF)
				data = tblName[0][addr & 0x03FF];
			if (addr >= 0x0400 && addr <= 0x07FF)
				data = tblName[0][addr & 0x03FF];
			if (addr >= 0x0800 && addr <= 0x0BFF)
				data = tblName[1][addr & 0x03FF];
			if (addr >= 0x0C00 && addr <= 0x0FFF)
				data = tblName[1][addr & 0x03FF];
		}
	}
	else if (addr >= 0x3F00 && addr <= 0x3FFF)
	{
		addr &= 0x001F;
		if (addr == 0x0010) addr = 0x0000;
		if (addr == 0x0014) addr = 0x0004;
		if (addr == 0x0018) addr = 0x0008;
		if (addr == 0x001C) addr = 0x000C;
		data = tblPalette[addr] & (mask.grayscale ? 0x30 : 0x3F);
	}

	return data;
}

void PPU::ppu_write(uint16_t addr, uint8_t data)
{
	addr &= 0x3FFF;

	if (cart->ppu_write(addr, data))
	{

	}
	else if (addr >= 0x0000 && addr <= 0x1FFF)
	{
		tblPattern[(addr & 0x1000) >> 12][addr & 0x0FFF] = data;
	}
	else if (addr >= 0x2000 && addr <= 0x3EFF)
	{
		addr &= 0x0FFF;
		if (cart->mirror == Cartridge::MIRROR::VERTICAL)
		{
			// Vertical
			if (addr >= 0x0000 && addr <= 0x03FF)
				tblName[0][addr & 0x03FF] = data;
			if (addr >= 0x0400 && addr <= 0x07FF)
				tblName[1][addr & 0x03FF] = data;
			if (addr >= 0x0800 && addr <= 0x0BFF)
				tblName[0][addr & 0x03FF] = data;
			if (addr >= 0x0C00 && addr <= 0x0FFF)
				tblName[1][addr & 0x03FF] = data;
		}
		else if (cart->mirror == Cartridge::MIRROR::HORIZONTAL)
		{
			// Horizontal
			if (addr >= 0x0000 && addr <= 0x03FF)
				tblName[0][addr & 0x03FF] = data;
			if (addr >= 0x0400 && addr <= 0x07FF)
				tblName[0][addr & 0x03FF] = data;
			if (addr >= 0x0800 && addr <= 0x0BFF)
				tblName[1][addr & 0x03FF] = data;
			if (addr >= 0x0C00 && addr <= 0x0FFF)
				tblName[1][addr & 0x03FF] = data;
		}
	}
	else if (addr >= 0x3F00 && addr <= 0x3FFF)
	{
		addr &= 0x001F;
		if (addr == 0x0010) addr = 0x0000;
		if (addr == 0x0014) addr = 0x0004;
		if (addr == 0x0018) addr = 0x0008;
		if (addr == 0x001C) addr = 0x000C;
		tblPalette[addr] = data;
	}
}

void PPU::connect_cartridge(const std::shared_ptr<Cartridge>& cartridge)
{
	this->cart = cartridge;
}

void PPU::reset()
{
	fine_x = 0x00;
	address_latch = 0x00;
	ppu_data_buffer = 0x00;
	scanline = 0;
	cycle = 0;
	bg_next_tile_id = 0x00;
	bg_next_tile_attrib = 0x00;
	bg_next_tile_lsb = 0x00;
	bg_next_tile_msb = 0x00;
	bg_shifter_pattern_lo = 0x0000;
	bg_shifter_pattern_hi = 0x0000;
	bg_shifter_attrib_lo = 0x0000;
	bg_shifter_attrib_hi = 0x0000;
	status.reg = 0x00;
	mask.reg = 0x00;
	control.reg = 0x00;
	vram_addr.reg = 0x0000;
	tram_addr.reg = 0x0000;
}

void PPU::set_pixel(TArray<FColor>& display, int r, int c, FColor pixel, int id) {
	// id identifies if we are setting a pixel on the screen, pattern table, or nametable

	int idx = -1;
	int width;
	int height;
	if (id == 0) {
		width = screen_width;
		height = screen_height;
		idx = width * r + c;
	}
	else if (id == 1) {
		width = nametable_width;
		height = nametable_height;
		idx = width * r + c;
	}
	else if (id == 2) {
		width = patterntable_width;
		height = patterntable_height;
		idx = width * r + c;
	}

	else { check(false); }
	if (idx < 0 || idx >= display.Num()) {
		return;
	}


	display[idx] = pixel;

}

TArray<FColor>& PPU::get_screen()
{
	return screen;
}

void PPU::clock()
{
	auto IncrementScrollX = [&]()
		{
			if (mask.render_background || mask.render_sprites)
			{
				if (vram_addr.coarse_x == 31)
				{
					vram_addr.coarse_x = 0;
					vram_addr.nametable_x = ~vram_addr.nametable_x;
				}
				else
				{
					vram_addr.coarse_x++;
				}
			}
		};

	auto IncrementScrollY = [&]()
		{
			if (mask.render_background || mask.render_sprites)
			{
				// If possible, just increment the fine y offset
				if (vram_addr.fine_y < 7)
				{
					vram_addr.fine_y++;
				}
				else
				{
					vram_addr.fine_y = 0;

					if (vram_addr.coarse_y == 29)
					{
						vram_addr.coarse_y = 0;
						vram_addr.nametable_y = ~vram_addr.nametable_y;
					}
					else if (vram_addr.coarse_y == 31)
					{
						vram_addr.coarse_y = 0;
					}
					else
					{
						vram_addr.coarse_y++;
					}
				}
			}
		};

	auto TransferAddressX = [&]()
		{
			// Ony if rendering is enabled
			if (mask.render_background || mask.render_sprites)
			{
				vram_addr.nametable_x = tram_addr.nametable_x;
				vram_addr.coarse_x = tram_addr.coarse_x;
			}
		};

	auto TransferAddressY = [&]()
		{
			// Ony if rendering is enabled
			if (mask.render_background || mask.render_sprites)
			{
				vram_addr.fine_y = tram_addr.fine_y;
				vram_addr.nametable_y = tram_addr.nametable_y;
				vram_addr.coarse_y = tram_addr.coarse_y;
			}
		};

	auto LoadBackgroundShifters = [&]()
		{
			bg_shifter_pattern_lo = (bg_shifter_pattern_lo & 0xFF00) | bg_next_tile_lsb;
			bg_shifter_pattern_hi = (bg_shifter_pattern_hi & 0xFF00) | bg_next_tile_msb;
			bg_shifter_attrib_lo = (bg_shifter_attrib_lo & 0xFF00) | ((bg_next_tile_attrib & 0b01) ? 0xFF : 0x00);
			bg_shifter_attrib_hi = (bg_shifter_attrib_hi & 0xFF00) | ((bg_next_tile_attrib & 0b10) ? 0xFF : 0x00);
		};

	auto UpdateShifters = [&]()
		{
			if (mask.render_background)
			{
				bg_shifter_pattern_lo <<= 1;
				bg_shifter_pattern_hi <<= 1;
				bg_shifter_attrib_lo <<= 1;
				bg_shifter_attrib_hi <<= 1;
			}

			if (mask.render_sprites && cycle >= 1 && cycle < 258)
			{
				for (int i = 0; i < sprite_count; i++)
				{
					if (spriteScanline[i].x > 0)
					{
						spriteScanline[i].x--;
					}
					else
					{
						sprite_shifter_pattern_lo[i] <<= 1;
						sprite_shifter_pattern_hi[i] <<= 1;
					}
				}
			}
		};


	if (scanline >= -1 && scanline < 240)
	{

		if (scanline == 0 && cycle == 0)
		{
			cycle = 1;
		}

		if (scanline == -1 && cycle == 1)
		{
			status.vertical_blank = 0;

			status.sprite_overflow = 0;

			status.sprite_zero_hit = 0;

			// Clear Shifters
			for (int i = 0; i < 8; i++)
			{
				sprite_shifter_pattern_lo[i] = 0;
				sprite_shifter_pattern_hi[i] = 0;
			}
		}


		if ((cycle >= 2 && cycle < 258) || (cycle >= 321 && cycle < 338))
		{
			UpdateShifters();


			switch ((cycle - 1) % 8)
			{
			case 0:

				LoadBackgroundShifters();

				bg_next_tile_id = ppu_read(0x2000 | (vram_addr.reg & 0x0FFF));

				break;
			case 2:
				bg_next_tile_attrib = ppu_read(0x23C0 | (vram_addr.nametable_y << 11)
					| (vram_addr.nametable_x << 10)
					| ((vram_addr.coarse_y >> 2) << 3)
					| (vram_addr.coarse_x >> 2));

				// palette selected. So shift as required...				
				if (vram_addr.coarse_y & 0x02) bg_next_tile_attrib >>= 4;
				if (vram_addr.coarse_x & 0x02) bg_next_tile_attrib >>= 2;
				bg_next_tile_attrib &= 0x03;
				break;

				// Compared to the last two, the next two are the easy ones... :P

			case 4:

				bg_next_tile_lsb = ppu_read((control.pattern_background << 12)
					+ ((uint16_t)bg_next_tile_id << 4)
					+ (vram_addr.fine_y) + 0);

				break;
			case 6:

				bg_next_tile_msb = ppu_read((control.pattern_background << 12)
					+ ((uint16_t)bg_next_tile_id << 4)
					+ (vram_addr.fine_y) + 8);
				break;
			case 7:

				IncrementScrollX();
				break;
			}
		}

		if (cycle == 256)
		{
			IncrementScrollY();
		}

		//...and reset the x position
		if (cycle == 257)
		{
			LoadBackgroundShifters();
			TransferAddressX();
		}

		// Superfluous reads of tile id at end of scanline
		if (cycle == 338 || cycle == 340)
		{
			bg_next_tile_id = ppu_read(0x2000 | (vram_addr.reg & 0x0FFF));
		}

		if (scanline == -1 && cycle >= 280 && cycle < 305)
		{
			// End of vertical blank period so reset the Y address ready for rendering
			TransferAddressY();
		}

		if (cycle == 257 && scanline >= 0)
		{

			std::memset(spriteScanline, 0xFF, 8 * sizeof(sObjectAttributeEntry));

			sprite_count = 0;

			// Secondly, clear out any residual information in sprite pattern shifters
			for (uint8_t i = 0; i < 8; i++)
			{
				sprite_shifter_pattern_lo[i] = 0;
				sprite_shifter_pattern_hi[i] = 0;
			}
			uint8_t nOAMEntry = 0;

			// New set of sprites. Sprite zero may not exist in the new set, so clear this
			// flag.
			bSpriteZeroHitPossible = false;

			while (nOAMEntry < 64 && sprite_count < 9)
			{
				// Note the conversion to signed numbers here
				int16_t diff = ((int16_t)scanline - (int16_t)OAM[nOAMEntry].y);


				if (diff >= 0 && diff < (control.sprite_size ? 16 : 8))
				{

					if (sprite_count < 8)
					{
						// Is this sprite sprite zero?
						if (nOAMEntry == 0)
						{
							bSpriteZeroHitPossible = true;
						}

						memcpy(&spriteScanline[sprite_count], &OAM[nOAMEntry], sizeof(sObjectAttributeEntry));
						sprite_count++;
					}
				}

				nOAMEntry++;
			} // End of sprite evaluation for next scanline

			status.sprite_overflow = (sprite_count > 8);

		}

		if (cycle == 340)
		{

			for (uint8_t i = 0; i < sprite_count; i++)
			{


				uint8_t sprite_pattern_bits_lo, sprite_pattern_bits_hi;
				uint16_t sprite_pattern_addr_lo, sprite_pattern_addr_hi;


				if (!control.sprite_size)
				{
					if (!(spriteScanline[i].attribute & 0x80))
					{
						sprite_pattern_addr_lo =
							(control.pattern_sprite << 12)
							| (spriteScanline[i].id << 4)
							| (scanline - spriteScanline[i].y);

					}
					else
					{
						sprite_pattern_addr_lo =
							(control.pattern_sprite << 12)
							| (spriteScanline[i].id << 4)
							| (7 - (scanline - spriteScanline[i].y));
					}

				}
				else
				{
					if (!(spriteScanline[i].attribute & 0x80))
					{
						// Sprite is NOT flipped vertically, i.e. normal
						if (scanline - spriteScanline[i].y < 8)
						{
							// Reading Top half Tile
							sprite_pattern_addr_lo =
								((spriteScanline[i].id & 0x01) << 12)
								| ((spriteScanline[i].id & 0xFE) << 4)
								| ((scanline - spriteScanline[i].y) & 0x07);
						}
						else
						{
							// Reading Bottom Half Tile
							sprite_pattern_addr_lo =
								((spriteScanline[i].id & 0x01) << 12)
								| (((spriteScanline[i].id & 0xFE) + 1) << 4)
								| ((scanline - spriteScanline[i].y) & 0x07);
						}
					}
					else
					{
						// Sprite is flipped vertically, i.e. upside down
						if (scanline - spriteScanline[i].y < 8)
						{
							// Reading Top half Tile
							sprite_pattern_addr_lo =
								((spriteScanline[i].id & 0x01) << 12)
								| (((spriteScanline[i].id & 0xFE) + 1) << 4)
								| (7 - (scanline - spriteScanline[i].y) & 0x07);
						}
						else
						{
							// Reading Bottom Half Tile
							sprite_pattern_addr_lo =
								((spriteScanline[i].id & 0x01) << 12)
								| ((spriteScanline[i].id & 0xFE) << 4)
								| (7 - (scanline - spriteScanline[i].y) & 0x07);
						}
					}
				}


				sprite_pattern_addr_hi = sprite_pattern_addr_lo + 8;
				sprite_pattern_bits_lo = ppu_read(sprite_pattern_addr_lo);
				sprite_pattern_bits_hi = ppu_read(sprite_pattern_addr_hi);

				if (spriteScanline[i].attribute & 0x40)
				{
					auto flipbyte = [](uint8_t b)
						{
							b = (b & 0xF0) >> 4 | (b & 0x0F) << 4;
							b = (b & 0xCC) >> 2 | (b & 0x33) << 2;
							b = (b & 0xAA) >> 1 | (b & 0x55) << 1;
							return b;
						};

					sprite_pattern_bits_lo = flipbyte(sprite_pattern_bits_lo);
					sprite_pattern_bits_hi = flipbyte(sprite_pattern_bits_hi);
				}

				sprite_shifter_pattern_lo[i] = sprite_pattern_bits_lo;
				sprite_shifter_pattern_hi[i] = sprite_pattern_bits_hi;
			}
		}
	}

	if (scanline == 240)
	{
		// Post Render Scanline, dont do anything
	}

	if (scanline >= 241 && scanline < 261)
	{
		if (scanline == 241 && cycle == 1)
		{
			status.vertical_blank = 1;
			if (control.enable_nmi)
				nmi = true;
		}
	}


	uint8_t bg_pixel = 0x00;
	uint8_t bg_palette = 0x00;
	if (mask.render_background)
	{
		uint16_t bit_mux = 0x8000 >> fine_x;

		// Select Plane pixels by extracting from the shifter 
		// at the required location. 
		uint8_t p0_pixel = (bg_shifter_pattern_lo & bit_mux) > 0;
		uint8_t p1_pixel = (bg_shifter_pattern_hi & bit_mux) > 0;

		// Combine to form pixel index
		bg_pixel = (p1_pixel << 1) | p0_pixel;

		// Get palette
		uint8_t bg_pal0 = (bg_shifter_attrib_lo & bit_mux) > 0;
		uint8_t bg_pal1 = (bg_shifter_attrib_hi & bit_mux) > 0;
		bg_palette = (bg_pal1 << 1) | bg_pal0;
	}

	uint8_t fg_pixel = 0x00;
	uint8_t fg_palette = 0x00;
	uint8_t fg_priority = 0x00;

	if (mask.render_sprites)
	{

		bSpriteZeroBeingRendered = false;

		for (uint8_t i = 0; i < sprite_count; i++)
		{
			// Scanline cycle has "collided" with sprite, shifters taking over
			if (spriteScanline[i].x == 0)
			{

				uint8_t fg_pixel_lo = (sprite_shifter_pattern_lo[i] & 0x80) > 0;
				uint8_t fg_pixel_hi = (sprite_shifter_pattern_hi[i] & 0x80) > 0;
				fg_pixel = (fg_pixel_hi << 1) | fg_pixel_lo;

				fg_palette = (spriteScanline[i].attribute & 0x03) + 0x04;
				fg_priority = (spriteScanline[i].attribute & 0x20) == 0;

				if (fg_pixel != 0)
				{
					if (i == 0) // Is this sprite zero?
					{
						bSpriteZeroBeingRendered = true;
					}

					break;
				}
			}
		}
	}


	uint8_t pixel = 0x00;
	uint8_t palette = 0x00;

	if (bg_pixel == 0 && fg_pixel == 0)
	{
		pixel = 0x00;
		palette = 0x00;
	}
	else if (bg_pixel == 0 && fg_pixel > 0)
	{
		pixel = fg_pixel;
		palette = fg_palette;
	}
	else if (bg_pixel > 0 && fg_pixel == 0)
	{
		pixel = bg_pixel;
		palette = bg_palette;
	}
	else if (bg_pixel > 0 && fg_pixel > 0)
	{
		if (fg_priority)
		{
			pixel = fg_pixel;
			palette = fg_palette;
		}
		else
		{
			pixel = bg_pixel;
			palette = bg_palette;
		}

		if (bSpriteZeroHitPossible && bSpriteZeroBeingRendered)
		{
			if (mask.render_background & mask.render_sprites)
			{
				if (~(mask.render_background_left | mask.render_sprites_left))
				{
					if (cycle >= 9 && cycle < 258)
					{
						status.sprite_zero_hit = 1;
					}
				}
				else
				{
					if (cycle >= 1 && cycle < 258)
					{
						status.sprite_zero_hit = 1;
					}
				}
			}
		}
	}

	set_pixel(screen, cycle - 1, scanline, get_colour_from_palette_ram(palette, pixel), 0);
	cycle++;
	if (cycle >= 341)
	{
		cycle = 0;
		scanline++;
		if (scanline >= 261)
		{
			scanline = -1;
			frame_complete = true;
		}
	}
}

