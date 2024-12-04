// Fill out your copyright notice in the Description page of Project Settings.

#include "mapper.h"

Mapper::Mapper(uint8_t pbanks, uint8_t cbanks) {
	prgbanks = pbanks;
	chrbanks = cbanks;

	reset();
}

Mapper::~Mapper()
{
}

void Mapper::reset()
{

}