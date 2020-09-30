--[[
CHIP-8 emulator for Lua

MIT License

Copyright (c) 2020 Celabyte

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
]]

-------------------------------------------------------------------------------

local bit = bit32 or require "bit"

local bit_lshift = bit.lshift
local bit_rshift = bit.rshift
local bit_and    = bit.band
local bit_or     = bit.bor
local bit_xor    = bit.bxor
local bit_not    = bit.bnot

local table_unpack = table.unpack or unpack

local math_random = math.random

-------------------------------------------------------------------------------

local chip={
	screen_width       = 64,
	screen_height      = 32,
	font_start_address = 0x050,
	font_end_address   = 0x0A0,
	rom_start_address  = 0x200,
	rom_end_address    = 0xFFF
}

chip.__index=chip

-------------------------------------------------------------------------------

function chip.new()
	return setmetatable({
		memory = {},     --4096 bytes
		screen = {},     --64x32 bits (unless super chip)
		stack  = {},     --16 levels 2 byte addressing
		input  = {},     --16 keys
		reg    = {},     --16 general purpose 1 byte registers
		id     = 0x000,  --Index register
		pc     = 0x000,  --Program counter
		delay  = 0x00,   --Delay timer
		sound  = 0x00,   --Sound timer
		op     = 0x0000, --2 byte opcode
		sp     = 0x0000, --2 byte stack pointer
		df     = 0       --Draw flag
	},chip)
end

function chip.init(vm)
	--Reset registers
	vm.pc = chip.rom_start_address
	vm.id = 0x000
	vm.sp = 0x0000
	vm.op = 0x0000
	
	--Reset timers
	vm.sound = 0x00
	vm.delay = 0x00
	
	--Clear memory
	for i=1,4096 do
		vm.memory[i]=0x00
	end
	
	--Clear screen
	for i=1,chip.screen_width*chip.screen_height do
		vm.screen[i]=0
	end
	
	--Clear stack
	for i=1,16 do
		vm.stack[i]=0x0000
	end
	
	--Clear general purpose registers
	for i=1,16 do
		vm.reg[i]=0x00
	end
	
	--Load font into memory
	for i=1,#chip.font_set do
		vm.memory[i]=chip.font_set[i]
	end
end

function chip.load(vm,data)
	assert(
		#data<=chip.rom_end_address-chip.rom_start_address,
		"ROM data is too big!"
	)
	for i=1,#data do
		vm.memory[chip.rom_start_address+i]=data:sub(i,i):byte()
	end
end

function chip.step(vm)
	--Fetch opcode
	vm.df = 0
	vm.op = bit_or(bit_lshift(vm.memory[vm.pc+1],8),vm.memory[vm.pc+2])
	
	--Execute opcode
	local instruction=chip.resolve_opcode(vm,chip.opcodes)
	if instruction then
		instruction(vm)
	else
		print(("Illegal instruction: %x"):format(vm.op))
	end
	
	--Decrement delay timer
	if vm.delay>0 then
		vm.delay=vm.delay-1
	end
	
	--Decrement sound timer
	if vm.sound>0 then
		if vm.sound==1 and vm.beep then
			vm.beep()
		end
		vm.sound=vm.sound-1
	end
end

function chip.resolve_opcode(vm,op_)
	if type(op_)=="function" then
		return op_
	else
		local opcode=bit_and(vm.op,op_[1])
		for code,op__ in pairs(op_[2]) do
			if opcode==code then
				local instruction=chip.resolve_opcode(vm,op__)
				if instruction then
					return instruction
				end
			end
		end
	end
end

function chip.debug(vm)
	print(("OP: %x, PC: %x, ID: %x, SP: %x"):format(
		vm.op,vm.pc,vm.id,vm.sp
	))
	print("REG: "..("%x "):rep(16):format(table_unpack(vm.reg)))
end

-------------------------------------------------------------------------------

chip.opcodes={0xF000,{
	[0x0000]={0x000F,{
		--[[
		Clear screen
		]]
		[0x0000]=function(vm)
			for i=1,chip.screen_width*chip.screen_height do
				vm.screen[i]=0
			end
			vm.df=1
			vm.pc=vm.pc+2
		end,
		
		--[[
		Return from subroutine
		]]
		[0x000E]=function(vm)
			vm.sp=vm.sp-1
			vm.pc=vm.stack[vm.sp+1]+2
		end
	}},
	
	--[[
	Jump to address NNN
	]]
	[0x1000]=function(vm)
		vm.pc=bit_and(vm.op,0x0FFF)
	end,
	
	--[[
	Call subroutine at address NNN
	]]
	[0x2000]=function(vm)
		vm.stack[vm.sp+1]=vm.pc
		vm.sp=vm.sp+1
		vm.pc=bit_and(vm.op,0x0FFF)
	end,
	
	--[[
	Skip next instruction if Vx equals NN
	]]
	[0x3000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		if vm.reg[vx]==bit_and(vm.op,0x00FF) then
			vm.pc=vm.pc+4
		else
			vm.pc=vm.pc+2
		end
	end,
	
	--[[
	Skip next instruction if Vx does not equal NN
	]]
	[0x4000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		if vm.reg[vx]~=bit_and(vm.op,0x00FF) then
			vm.pc=vm.pc+4
		else
			vm.pc=vm.pc+2
		end
	end,
	
	--[[
	Skip next instruction if Vx equals Vy
	]]
	[0x5000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
		if vm.reg[vx]==vm.reg[vy] then
			vm.pc=vm.pc+4
		else
			vm.pc=vm.pc+2
		end
	end,
	
	--[[
	Set Vx to NN
	]]
	[0x6000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		vm.reg[vx]=bit_and(vm.op,0x00FF)
		vm.pc=vm.pc+2
	end,
	
	--[[
	Add NN to Vx
	Does not affect Vf
	]]
	[0x7000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		local vr=vm.reg[vx]+bit_and(vm.op,0x00FF)
		vm.reg[vx]=vr%0x100
		vm.pc=vm.pc+2
	end,
	
	[0x8000]={0x000F,{
		--[[
		Set register Vx to Vy
		]]
		[0x0000]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			vm.reg[vx]=vm.reg[vy]
			vm.pc=vm.pc+2
		end,
		
		--[[
		Perform bitwise OR to Vx with Vy
		]]
		[0x0001]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			vm.reg[vx]=bit_or(vm.reg[vx],vm.reg[vy])
			vm.pc=vm.pc+2
		end,
		
		--[[
		Perform bitwise AND to Vx with Vy 
		]]
		[0x0002]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			vm.reg[vx]=bit_and(vm.reg[vx],vm.reg[vy])
			vm.pc=vm.pc+2
		end,
		
		--[[
		Perform bitwise XOR to Vx with Vy
		]]
		[0x0003]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			vm.reg[vx]=bit_xor(vm.reg[vx],vm.reg[vy])
			vm.pc=vm.pc+2
		end,
		
		--[[
		Add Vy to VX
		Set Vf to 1 if carry, else 0
		]]
		[0x0004]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			local vr=vm.reg[vx]+vm.reg[vy]
			vm.reg[vx]=vr%0x100
			vm.reg[0xF]=vr>0xFF and 1 or 0
			vm.pc=vm.pc+2
		end,
		
		--[[
		Subtract Vx by Vy
		Set Vf to 0 if borrow, else 1
		]]
		[0x0005]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			local vr=vm.reg[vx]-vm.reg[vy]
			vm.reg[0xF]=vm.reg[vx]>vm.reg[vy] and 1 or 0
			vm.reg[vx]=vr%0x100
			vm.pc=vm.pc+2
		end,
		
		--[[
		Store least significant bit of Vx in Vf
		Bit shift Vx to the right by 1
		]]
		[0x0006]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.reg[0xF]=bit_and(vm.reg[vx],0x1)
			vm.reg[vx]=bit_rshift(vm.reg[vx],1)
			vm.pc=vm.pc+2
		end,
		
		--[[
		Set Vx to Vy minus Vx
		Set Vf to 0 if borrow, else 1
		]]
		[0x0007]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
			local vr=vm.reg[vy]-vm.reg[vx]
			vm.reg[0xF]=vm.reg[vy]>vm.reg[vx] and 1 or 0
			vm.reg[vx]=vr%0x100
			vm.pc=vm.pc+2
		end,
		
		--[[
		Store most significant bit of Vx in Vf
		Bit shift Vx to the right by 1
		]]
		[0x000E]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.reg[0xF]=bit_and(vm.reg[vx],0x80)
			vm.reg[vx]=bit_lshift(vm.reg[vx],1)
			vm.pc=vm.pc+2
		end
	}},
	
	--[[
	Skip next instruction if Vx does not equal Vy
	]]
	[0x9000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
		if vm.reg[vx]~=vm.reg[vy] then
			vm.pc=vm.pc+4
		else
			vm.pc=vm.pc+2
		end
	end,
	
	--[[
	Set index register to NNN
	]]
	[0xA000]=function(vm)
		vm.id=bit_and(vm.op,0x0FFF)
		vm.pc=vm.pc+2
	end,
	
	--[[
	Jump to address NNN+V0
	]]
	[0xB000]=function(vm)
		vm.pc=bit_and(vm.op,0x0FFF)+vm.reg[1]
	end,
	
	--[[
	Set Vx to bitwise AND of random and NN
	]]
	[0xC000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		vm.reg[vx]=bit_and(
			math_random(0,0xFF),
			bit_and(vm.op,0x00FF)
		)
		vm.pc=vm.pc+2
	end,
	
	--[[
	Draw sprite at Vx,Vy
	Width of 8 pixels and Height of N+1
	]]
	[0xD000]=function(vm)
		local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
		local vy=bit_rshift(bit_and(vm.op,0x00F0),4)+1
		local ox=vm.reg[vx]
		local oy=vm.reg[vy]
		
		vm.reg[0xF]=0
		
		for x=0,7 do
			for y=0,bit_and(vm.op,0x000F)+1 do
				local px=(ox+x)%chip.screen_width
				local py=(oy+y)%chip.screen_height
				local p=chip.screen_height*(py+1)+px
				if vm.screen[p] then
					local set=vm.screen[p]>0
					vm.screen[p]=bit_xor(
						vm.screen[p],
						bit_and(
							vm.memory[vm.id+1+y],
							bit_rshift(0x80,x)
						)
					)
					if set and vm.screen[p]==0 then
						vm.reg[0xF]=1
					end
				end
			end
		end
		
		vm.df=1
		vm.pc=vm.pc+2
	end,
	
	[0xE000]={0x00FF,{
		--[[
		Skip next instruction if key in Vx is pressed
		]]
		[0x009E]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			if vm.input[vm.reg[vx]]>0 then
				vm.pc=vm.pc+4
			else
				vm.pc=vm.pc+2
			end
		end,
		
		--[[
		Skip next instruction if key in Vx isn't pressed
		]]
		[0x00A1]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			if vm.input[vm.reg[vx]]==0 then
				vm.pc=vm.pc+4
			else
				vm.pc=vm.pc+2
			end
		end
	}},
	[0xF000]={0x00FF,{
		--[[
		Set Vx to value of delay timer
		]]
		[0x0007]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.reg[vx]=vm.delay
			vm.pc=vm.pc+2
		end,
		
		--[[
		Halt until key press
		Set Vx to key
		]]
		[0x000A]=function(vm)
			--Todo
			
		end,
		
		--[[
		Set delay timer to Vx
		]]
		[0x0015]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.delay=vm.reg[vx]
			vm.pc=vm.pc+2
		end,
		
		--[[
		Set sound timer to Vx
		]]
		[0x0018]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.sound=vm.reg[vx]
			vm.pc=vm.pc+2
		end,
		
		--[[
		Add Vx to id
		Does not affect Vf
		]]
		[0x0029]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.id=vm.id+vm.reg[vx]
			vm.pc=vm.pc+2
		end,
		
		--[[
		Store BCD representation of Vx at memory id,id+1,id+2
		]]
		[0x0033]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			vm.memory[vm.id+1] = vm.reg[vx]/100
			vm.memory[vm.id+2] = (vm.reg[vx]/10)%10
			vm.memory[vm.id+3] = (vm.reg[vx]/100)%10
			vm.pc=vm.pc+2
		end,
		
		--[[
		Store registers V0-Vx to memory at address id
		]]
		[0x0055]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			for i=1,vm.reg[vx] do
				vm.memory[vm.id+i]=vm.reg[i]
			end
			vm.pc=vm.pc+2
		end,
		
		--[[
		Load memory from address id to registers V0-Vx
		]]
		[0x0065]=function(vm)
			local vx=bit_rshift(bit_and(vm.op,0x0F00),8)+1
			for i=1,vm.reg[vx] do
				vm.reg[i]=vm.memory[vm.id+i]
			end
			vm.pc=vm.pc+2
		end
	}}
}}

-------------------------------------------------------------------------------

chip.font_set={
	0xF0, 0x90, 0x90, 0x90, 0xF0, --0
	0x20, 0x60, 0x20, 0x20, 0x70, --1
	0xF0, 0x10, 0xF0, 0x80, 0xF0, --2
	0xF0, 0x10, 0xF0, 0x10, 0xF0, --3
	0x90, 0x90, 0xF0, 0x10, 0x10, --4
	0xF0, 0x80, 0xF0, 0x10, 0xF0, --5
	0xF0, 0x80, 0xF0, 0x90, 0xF0, --6
	0xF0, 0x10, 0x20, 0x40, 0x40, --7
	0xF0, 0x90, 0xF0, 0x90, 0xF0, --8
	0xF0, 0x90, 0xF0, 0x10, 0xF0, --9
	0xF0, 0x90, 0xF0, 0x90, 0x90, --A
	0xE0, 0x90, 0xE0, 0x90, 0xE0, --B
	0xF0, 0x80, 0x80, 0x80, 0xF0, --C
	0xE0, 0x90, 0x90, 0x90, 0xE0, --D
	0xF0, 0x80, 0xF0, 0x80, 0xF0, --E
	0xF0, 0x80, 0xF0, 0x80, 0x80  --F
}

-------------------------------------------------------------------------------

return chip