/*
 *********************************************
 *  314 Principles of Programming Languages  *
 *  Spring 2014                              *
 *  Authors: Ulrich Kremer                   *
 *           Hans Christian Woithe           *
 *********************************************
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "InstrUtils.h"
#include "Utils.h"

 /* Routines for dead code optimization */
static void markCriticals(Instruction *instr);
static void	criticalVariable(Instruction *instr, char variable);
static void	criticalRegister(Instruction *instr, int reg);
static void	deleteNonCriticals(Instruction *instr);

static void markCriticals(Instruction *instr) 
{
	/* all read and write instructions are critical */
	while(instr) {
		switch (instr->opcode) {
		case READ:
			instr->critical = 1;
			break;
		case WRITE:
			instr->critical = 1;
			criticalVariable(instr, instr->field1);
			break;
		default: 
			break;
		}
		instr = instr->next;
	}
}

static void	criticalVariable(Instruction *instr, char variable)
{
	/* begin traversing through instruction list to find critical STORE or LOAD instr */
	instr = instr->prev;
	while (instr) {
		if (instr->opcode == STORE) {
			if (instr->field1 == variable) {
				instr->critical = 1; // found critical store instr
				criticalRegister(instr, instr->field2);
				break;
				/* find critical register for store! */
			}
		}
		/*
		if (instr->opcode == LOAD) {
			if (instr->field2 == variable) {
				instr->critical = 1;
				criticalVariable(instr, instr->field2);
				break;
			}	
		}*/
		instr = instr->prev;
	}
}


static void	criticalRegister(Instruction *instr, int reg)
{
	/* critical register (reg), mark most recent operation on reg as critical */
	/* looking for a LOAD, LOADI, ADD, SUB, or MUL that has target register == reg */

	/* begin traversing through instruction list to find critical operation */
	instr = instr->prev; 
	int stillLooking = 1;
	while (instr && stillLooking) {
		switch (instr->opcode) {
		case LOAD:
			if (instr->field1 == reg) {
				stillLooking = 0;
				instr->critical = 1; 
				criticalVariable(instr, instr->field2);
			}
			break;
		case LOADI:
			// LOAD and LOADI each generate only 1 new critical register
			if (instr->field1 == reg) {
				stillLooking = 0;
				instr->critical = 1; 
				//criticalRegister(instr, instr->field1);
			}
			break;
		case ADD:
		case SUB:
		case MUL:
			// ADD SUB and MUL each generate 2 new critical registers
			if (instr->field1 == reg) {
				stillLooking = 0;
				instr->critical = 1; 
				criticalRegister(instr, instr->field2); // first operand's register is now critical
				criticalRegister(instr, instr->field3); // second operand's register is now critical
			}
			break;
		case READ:
		case WRITE:
		case STORE:
			break;
		}

		instr = instr->prev;
	}
}

static void deleteNonCriticals(Instruction *instr) 
{
	Instruction *curr;
	Instruction *prev;
	while (instr) {
		if (instr->critical != 1) {
			// delete non critical instructions	
			curr = instr;
			prev = curr->prev;

			instr = instr->next;
			instr->prev = curr->prev;
			prev->next = instr;
			free(curr);

		} 
		else {
			// don't delete critical instructions
			instr = instr->next;
			continue;
		}
	}
}

int main()
{
	Instruction *head;

	head = ReadInstructionList(stdin);
	if (!head) {
		WARNING("No instructions\n");
		exit(EXIT_FAILURE);
	}

	markCriticals(head);
	deleteNonCriticals(head);

	if (head) {
		PrintInstructionList(stdout, head);
		DestroyInstructionList(head);
	}
	return EXIT_SUCCESS;
}

