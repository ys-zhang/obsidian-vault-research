---

excalidraw-plugin: raw

---
==⚠  Switch to EXCALIDRAW VIEW in the MORE OPTIONS menu of this document. ⚠==


# Text Elements
We present a learning
technique that uses neural networks to learn patterns in the
input files from past fuzzing explorations to guide future fuzzing
explorations. ^egJ0JKGO

use machine learning to learn a strategy for guiding the input mutations
based on previous history of executed inputs and code coverage
information. ^D9pLhKhV

Fuzzer ^WbBgcxIt

seed ^Gm840jKp

ML Model ^bRt4gacR

evaluate/choosing mutation that
maximize coverage gain ^L8PjESA2

How to choose mutations (where to mutate? mutate to what?)
    that MAXIMISE coverage gain? ^MWO76IFi

Fuzzer ^D85D88qC

seed ^1Jc646do

ML Model ^2xVRjfD9

mutation ^qY5HPizI

Target Program ^ETPGUyTA

Mutates good positions ^zK9Rqc46

mutates bad positions ^vmuwEcNW

How to assess input positions, where shall we mutate? ^HGDXzYvu

f(mutation|x) = Gain(coverage(x), coverage(mutate(x))) ^V6GvCBBd

this is what we want to learn ^WYF6gXul

How to define the gain of coverage? ^K2W19Hhs

Gain(b, b') = sum of new ones in the coverage bitmap ^j9FL9ewg

The goal of
augmented fuzzing is to improve the hit-rate of mutations.
Using the annotated seed provided by the model, mutations
that are unlikely to give input gain are avoided. 

We used a
highly permissive veto approach to reject mutations which
target no useful locations. ^bIORBHm7

why this works ^k0oo2n1V

%%
# Drawing
```json
{
	"type": "excalidraw",
	"version": 2,
	"source": "https://excalidraw.com",
	"elements": [
		{
			"type": "text",
			"version": 72,
			"versionNonce": 2140706217,
			"isDeleted": false,
			"id": "egJ0JKGO",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -347.33331298828125,
			"y": -282.4999694824219,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 558,
			"height": 91,
			"seed": 567108777,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "We present a learning\ntechnique that uses neural networks to learn patterns in the\ninput files from past fuzzing explorations to guide future fuzzing\nexplorations.",
			"rawText": "We present a learning\ntechnique that uses neural networks to learn patterns in the\ninput files from past fuzzing explorations to guide future fuzzing\nexplorations.",
			"baseline": 86,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 43,
			"versionNonce": 1169301385,
			"isDeleted": false,
			"id": "D9pLhKhV",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -344.6666259765625,
			"y": -139.83331298828125,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 629,
			"height": 68,
			"seed": 1416165991,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 2,
			"text": "use machine learning to learn a strategy for guiding the input mutations\nbased on previous history of executed inputs and code coverage\ninformation.",
			"rawText": "use machine learning to learn a strategy for guiding the input mutations\nbased on previous history of executed inputs and code coverage\ninformation.",
			"baseline": 64,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "rectangle",
			"version": 103,
			"versionNonce": 258595495,
			"isDeleted": false,
			"id": "9b_qeI8SoQoixA_wtK0eI",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 80,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 120,
			"height": 80,
			"seed": 1804955815,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"j_cUoyyGkmq9fN2rXGgPU"
			]
		},
		{
			"type": "text",
			"version": 80,
			"versionNonce": 649407785,
			"isDeleted": false,
			"id": "WbBgcxIt",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360,
			"y": 100,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 65,
			"height": 25,
			"seed": 1588196041,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "Fuzzer",
			"rawText": "Fuzzer",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 354,
			"versionNonce": 1291741833,
			"isDeleted": false,
			"id": "j_cUoyyGkmq9fN2rXGgPU",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -252.66066450467883,
			"y": 116.99953600280891,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 126.4495345212398,
			"height": 1.2449263438902989,
			"seed": 1767924969,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "9b_qeI8SoQoixA_wtK0eI",
				"gap": 7.339335495321152,
				"focus": -0.05758494535414237
			},
			"endBinding": {
				"elementId": "jX6HNUazpm-hRHdi7PiTS",
				"gap": 6.405731041482854,
				"focus": 0.11750305858618763
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					126.4495345212398,
					-1.2449263438902989
				]
			]
		},
		{
			"type": "ellipse",
			"version": 24,
			"versionNonce": 12760487,
			"isDeleted": false,
			"id": "jX6HNUazpm-hRHdi7PiTS",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -120,
			"y": 80,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 80,
			"height": 80,
			"seed": 976624231,
			"groupIds": [
				"g1_UmHBlbWinEl4aI8IY-"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"j_cUoyyGkmq9fN2rXGgPU",
				"w7zh155_i3UxnkjvGnvLz"
			]
		},
		{
			"type": "text",
			"version": 19,
			"versionNonce": 577560519,
			"isDeleted": false,
			"id": "Gm840jKp",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -102,
			"y": 107.5,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 44,
			"height": 25,
			"seed": 1094571785,
			"groupIds": [
				"g1_UmHBlbWinEl4aI8IY-"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "seed",
			"rawText": "seed",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 76,
			"versionNonce": 1781144649,
			"isDeleted": false,
			"id": "w7zh155_i3UxnkjvGnvLz",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -40,
			"y": 120,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 120,
			"height": 0,
			"seed": 1261446761,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "jX6HNUazpm-hRHdi7PiTS",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Xuv73aIpGzW8NemekRXd9",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					120,
					0
				]
			]
		},
		{
			"type": "diamond",
			"version": 26,
			"versionNonce": 551254121,
			"isDeleted": false,
			"id": "Xuv73aIpGzW8NemekRXd9",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 80,
			"y": 80,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 160,
			"height": 80,
			"seed": 88087049,
			"groupIds": [
				"oyVpz4Im7Be3kqFkmG3wi"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"w7zh155_i3UxnkjvGnvLz",
				"b75O3MZrlxCVM7BWNurnD"
			]
		},
		{
			"type": "text",
			"version": 28,
			"versionNonce": 1155708681,
			"isDeleted": false,
			"id": "bRt4gacR",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 114.5,
			"y": 107.5,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 91,
			"height": 25,
			"seed": 1541488457,
			"groupIds": [
				"oyVpz4Im7Be3kqFkmG3wi"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "ML Model",
			"rawText": "ML Model",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 12,
			"versionNonce": 1333631433,
			"isDeleted": false,
			"id": "b75O3MZrlxCVM7BWNurnD",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 160,
			"y": -40,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 20,
			"height": 120,
			"seed": 579909607,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "L8PjESA2",
				"focus": 0.9799498746867168,
				"gap": 1
			},
			"endBinding": {
				"elementId": "Xuv73aIpGzW8NemekRXd9",
				"focus": -0.33333333333333337,
				"gap": 8.94427190999916
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-20,
					120
				]
			]
		},
		{
			"type": "text",
			"version": 87,
			"versionNonce": 605586247,
			"isDeleted": false,
			"id": "L8PjESA2",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 160,
			"y": -60,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 324,
			"height": 51,
			"seed": 340849095,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"b75O3MZrlxCVM7BWNurnD"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "evaluate/choosing mutation that\nmaximize coverage gain",
			"rawText": "evaluate/choosing mutation that\nmaximize coverage gain",
			"baseline": 44,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 123,
			"versionNonce": 2079218281,
			"isDeleted": false,
			"id": "MWO76IFi",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 200,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 876,
			"height": 71,
			"seed": 2016420457,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "How to choose mutations (where to mutate? mutate to what?)\n    that MAXIMISE coverage gain?",
			"rawText": "How to choose mutations (where to mutate? mutate to what?)\n    that MAXIMISE coverage gain?",
			"baseline": 60,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 106,
			"versionNonce": 1279158183,
			"isDeleted": false,
			"id": "D85D88qC",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 420,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 65,
			"height": 25,
			"seed": 431261031,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "Fuzzer",
			"rawText": "Fuzzer",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 510,
			"versionNonce": 1651731561,
			"isDeleted": false,
			"id": "7bm8bVvNKn5JvoFZqD-Yx",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -272.66066450467883,
			"y": 436.9995360028089,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 126.4495345212398,
			"height": 1.2449263438902989,
			"seed": 1842227337,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "Sme3Evo6juuST2-97hO1l",
				"focus": -0.05758686262011673,
				"gap": 7.339335495321166
			},
			"endBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": 0.11750305858618851,
				"gap": 6.405731041482866
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					126.4495345212398,
					-1.2449263438902989
				]
			]
		},
		{
			"type": "ellipse",
			"version": 65,
			"versionNonce": 1834242983,
			"isDeleted": false,
			"id": "XfzGTTAEcEB-fmGHNpUBI",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -140,
			"y": 400,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 80,
			"height": 80,
			"seed": 1005404295,
			"groupIds": [
				"UFLWVbvjeFxuk_Ys1vjXj"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"7bm8bVvNKn5JvoFZqD-Yx",
				"5hBAnJFDYrPk8jmpNgLf6",
				"CtVs8edZv6lVajjuLkDJ0",
				"OCZDH5nr4gcsj_BXOykWT"
			]
		},
		{
			"type": "text",
			"version": 45,
			"versionNonce": 568124137,
			"isDeleted": false,
			"id": "1Jc646do",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -122,
			"y": 427.5,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 44,
			"height": 25,
			"seed": 887762793,
			"groupIds": [
				"UFLWVbvjeFxuk_Ys1vjXj"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "seed",
			"rawText": "seed",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 281,
			"versionNonce": 488883977,
			"isDeleted": false,
			"id": "5hBAnJFDYrPk8jmpNgLf6",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -59,
			"y": 440,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 118,
			"height": 0,
			"seed": 161585063,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": 0,
				"gap": 1
			},
			"endBinding": {
				"elementId": "azgaKWRDY1IdjAb2eXpWM",
				"focus": 0,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					118,
					0
				]
			]
		},
		{
			"type": "diamond",
			"version": 65,
			"versionNonce": 1482226281,
			"isDeleted": false,
			"id": "azgaKWRDY1IdjAb2eXpWM",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 60,
			"y": 400,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 160,
			"height": 80,
			"seed": 211966537,
			"groupIds": [
				"gxKJOKTEKjLCKTertl4FN"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"5hBAnJFDYrPk8jmpNgLf6",
				"ADeriNe-F1ONNsrtVWZUH",
				"OCZDH5nr4gcsj_BXOykWT",
				"IsMfRTnrbECgHH6nZQa6M"
			]
		},
		{
			"type": "text",
			"version": 54,
			"versionNonce": 1123408263,
			"isDeleted": false,
			"id": "2xVRjfD9",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 94.5,
			"y": 427.5,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 91,
			"height": 25,
			"seed": 1224608455,
			"groupIds": [
				"gxKJOKTEKjLCKTertl4FN"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "ML Model",
			"rawText": "ML Model",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "rectangle",
			"version": 125,
			"versionNonce": 1246278535,
			"isDeleted": false,
			"id": "Sme3Evo6juuST2-97hO1l",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -400,
			"y": 400,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 120,
			"height": 80,
			"seed": 566734825,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"j_cUoyyGkmq9fN2rXGgPU",
				"7bm8bVvNKn5JvoFZqD-Yx"
			]
		},
		{
			"type": "arrow",
			"version": 125,
			"versionNonce": 760836073,
			"isDeleted": false,
			"id": "CtVs8edZv6lVajjuLkDJ0",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -102.84641676604951,
			"y": 480.90107469974294,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 2.846416766049515,
			"height": 119.09892530025706,
			"seed": 964375337,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": 0.09557111227527659,
				"gap": 1
			},
			"endBinding": {
				"elementId": "UQjzUBZDl2R8pw8P25wcu",
				"focus": -0.1808472669483489,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					2.846416766049515,
					119.09892530025706
				]
			]
		},
		{
			"type": "ellipse",
			"version": 18,
			"versionNonce": 1001660999,
			"isDeleted": false,
			"id": "UQjzUBZDl2R8pw8P25wcu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -140,
			"y": 600,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 100,
			"height": 80,
			"seed": 950592553,
			"groupIds": [
				"8BLWf2G6WItGoMcS3yJ4O"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"CtVs8edZv6lVajjuLkDJ0",
				"ndM9E4X7g7-Y992ve3co2"
			]
		},
		{
			"type": "text",
			"version": 11,
			"versionNonce": 1296954121,
			"isDeleted": false,
			"id": "qY5HPizI",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -132.5,
			"y": 627.5,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 85,
			"height": 25,
			"seed": 258215593,
			"groupIds": [
				"8BLWf2G6WItGoMcS3yJ4O"
			],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "mutation",
			"rawText": "mutation",
			"baseline": 18,
			"textAlign": "center",
			"verticalAlign": "middle"
		},
		{
			"type": "arrow",
			"version": 51,
			"versionNonce": 583748009,
			"isDeleted": false,
			"id": "ndM9E4X7g7-Y992ve3co2",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -40,
			"y": 600,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 140,
			"height": 120,
			"seed": 1700316425,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "UQjzUBZDl2R8pw8P25wcu",
				"focus": 0.04873701788285782,
				"gap": 18.429078977666123
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					140,
					-120
				]
			]
		},
		{
			"type": "arrow",
			"version": 132,
			"versionNonce": 220645895,
			"isDeleted": false,
			"id": "OCZDH5nr4gcsj_BXOykWT",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 127.04167638452715,
			"y": 396.47916180773643,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 201.87495885523026,
			"height": 69.81261212511924,
			"seed": 132441383,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "azgaKWRDY1IdjAb2eXpWM",
				"focus": 0.5903157803210995,
				"gap": 8.94427190999916
			},
			"endBinding": {
				"elementId": "XfzGTTAEcEB-fmGHNpUBI",
				"focus": -0.6300776570342392,
				"gap": 12.434441832141289
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-96.54164586694903,
					-69.81261212511924
				],
				[
					-201.87495885523026,
					-2.4792381016817444
				]
			]
		},
		{
			"type": "arrow",
			"version": 177,
			"versionNonce": 1426587113,
			"isDeleted": false,
			"id": "IsMfRTnrbECgHH6nZQa6M",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 220.46605793766682,
			"y": 446.53059191131354,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 282.93408151006884,
			"height": 5.53249875621259,
			"seed": 1233056775,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "azgaKWRDY1IdjAb2eXpWM",
				"focus": 0.20260067091992187,
				"gap": 6.049566424818231
			},
			"endBinding": null,
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					282.93408151006884,
					-5.53249875621259
				]
			]
		},
		{
			"type": "text",
			"version": 31,
			"versionNonce": 234865993,
			"isDeleted": false,
			"id": "ETPGUyTA",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 520,
			"y": 420,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 158,
			"height": 25,
			"seed": 779805095,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "Target Program",
			"rawText": "Target Program",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 57,
			"versionNonce": 1014460873,
			"isDeleted": false,
			"id": "zK9Rqc46",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 260,
			"y": 460,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 185,
			"height": 20,
			"seed": 469730727,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 16,
			"fontFamily": 1,
			"text": "Mutates good positions",
			"rawText": "Mutates good positions",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 26,
			"versionNonce": 1668036329,
			"isDeleted": false,
			"id": "vmuwEcNW",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 80,
			"y": 320,
			"strokeColor": "#e67700",
			"backgroundColor": "transparent",
			"width": 176,
			"height": 20,
			"seed": 661792617,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 16,
			"fontFamily": 1,
			"text": "mutates bad positions",
			"rawText": "mutates bad positions",
			"baseline": 14,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 91,
			"versionNonce": 1991885959,
			"isDeleted": false,
			"id": "HGDXzYvu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360,
			"y": 720,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 755,
			"height": 35,
			"seed": 734204775,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "How to assess input positions, where shall we mutate?",
			"rawText": "How to assess input positions, where shall we mutate?",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 96,
			"versionNonce": 1456570153,
			"isDeleted": false,
			"id": "V6GvCBBd",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340,
			"y": 800,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 633,
			"height": 24,
			"seed": 1434029767,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Qu_NgoxeX66ajubSSCGZJ"
			],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "f(mutation|x) = Gain(coverage(x), coverage(mutate(x)))",
			"rawText": "f(mutation|x) = Gain(coverage(x), coverage(mutate(x)))",
			"baseline": 20,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 51,
			"versionNonce": 1108583017,
			"isDeleted": false,
			"id": "Qu_NgoxeX66ajubSSCGZJ",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -110.16355440082057,
			"y": 959,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 8.771849829179388,
			"height": 118.99999999999989,
			"seed": 415220935,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "WYF6gXul",
				"focus": -0.9159663865546218,
				"gap": 1
			},
			"endBinding": {
				"elementId": "V6GvCBBd",
				"focus": 0.5033860045146726,
				"gap": 6
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-8.771849829179388,
					-118.99999999999989
				]
			]
		},
		{
			"type": "text",
			"version": 52,
			"versionNonce": 654653415,
			"isDeleted": false,
			"id": "WYF6gXul",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -120,
			"y": 960,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 297,
			"height": 25,
			"seed": 1703926889,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"Qu_NgoxeX66ajubSSCGZJ"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "this is what we want to learn",
			"rawText": "this is what we want to learn",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 54,
			"versionNonce": 515902535,
			"isDeleted": false,
			"id": "K2W19Hhs",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -360,
			"y": 1040,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 501,
			"height": 35,
			"seed": 868563241,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 28,
			"fontFamily": 1,
			"text": "How to define the gain of coverage?",
			"rawText": "How to define the gain of coverage?",
			"baseline": 25,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 73,
			"versionNonce": 905460039,
			"isDeleted": false,
			"id": "j9FL9ewg",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -340,
			"y": 1100,
			"strokeColor": "#087f5b",
			"backgroundColor": "transparent",
			"width": 609,
			"height": 24,
			"seed": 33497737,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "Gain(b, b') = sum of new ones in the coverage bitmap",
			"rawText": "Gain(b, b') = sum of new ones in the coverage bitmap",
			"baseline": 20,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "text",
			"version": 16,
			"versionNonce": 1269140519,
			"isDeleted": false,
			"id": "bIORBHm7",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": -380,
			"y": 1220,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 680,
			"height": 192,
			"seed": 431084423,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"SYVAQpz7NfjL8vmOJffFu"
			],
			"fontSize": 20,
			"fontFamily": 3,
			"text": "The goal of\naugmented fuzzing is to improve the hit-rate of mutations.\nUsing the annotated seed provided by the model, mutations\nthat are unlikely to give input gain are avoided. \n\nWe used a\nhighly permissive veto approach to reject mutations which\ntarget no useful locations.",
			"rawText": "The goal of\naugmented fuzzing is to improve the hit-rate of mutations.\nUsing the annotated seed provided by the model, mutations\nthat are unlikely to give input gain are avoided. \n\nWe used a\nhighly permissive veto approach to reject mutations which\ntarget no useful locations.",
			"baseline": 188,
			"textAlign": "left",
			"verticalAlign": "top"
		},
		{
			"type": "arrow",
			"version": 160,
			"versionNonce": 1193658599,
			"isDeleted": false,
			"id": "SYVAQpz7NfjL8vmOJffFu",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 459,
			"y": 1337.3348642263772,
			"strokeColor": "#000000",
			"backgroundColor": "transparent",
			"width": 157.99999999999994,
			"height": 26.80271830461993,
			"seed": 1908752967,
			"groupIds": [],
			"strokeSharpness": "round",
			"boundElementIds": [],
			"startBinding": {
				"elementId": "k0oo2n1V",
				"focus": 0.29577464788732394,
				"gap": 1
			},
			"endBinding": {
				"elementId": "bIORBHm7",
				"focus": 0.6896551724137931,
				"gap": 1
			},
			"lastCommittedPoint": null,
			"startArrowhead": null,
			"endArrowhead": "arrow",
			"points": [
				[
					0,
					0
				],
				[
					-157.99999999999994,
					26.80271830461993
				]
			]
		},
		{
			"type": "text",
			"version": 30,
			"versionNonce": 1284575431,
			"isDeleted": false,
			"id": "k0oo2n1V",
			"fillStyle": "hachure",
			"strokeWidth": 1,
			"strokeStyle": "solid",
			"roughness": 1,
			"opacity": 100,
			"angle": 0,
			"x": 460,
			"y": 1320,
			"strokeColor": "#862e9c",
			"backgroundColor": "transparent",
			"width": 140,
			"height": 25,
			"seed": 1269211399,
			"groupIds": [],
			"strokeSharpness": "sharp",
			"boundElementIds": [
				"SYVAQpz7NfjL8vmOJffFu"
			],
			"fontSize": 20,
			"fontFamily": 1,
			"text": "why this works",
			"rawText": "why this works",
			"baseline": 18,
			"textAlign": "left",
			"verticalAlign": "top"
		}
	],
	"appState": {
		"theme": "light",
		"viewBackgroundColor": "#ffffff",
		"currentItemStrokeColor": "#862e9c",
		"currentItemBackgroundColor": "transparent",
		"currentItemFillStyle": "hachure",
		"currentItemStrokeWidth": 1,
		"currentItemStrokeStyle": "solid",
		"currentItemRoughness": 1,
		"currentItemOpacity": 100,
		"currentItemFontFamily": 1,
		"currentItemFontSize": 20,
		"currentItemTextAlign": "left",
		"currentItemStrokeSharpness": "sharp",
		"currentItemStartArrowhead": null,
		"currentItemEndArrowhead": "arrow",
		"currentItemLinearStrokeSharpness": "round",
		"gridSize": 20
	},
	"files": {}
}
```

# SVG snapshot
==⚠ Remove all linebreaks from SVG string before use. Linebreaks were added to improve markdown view speed. ⚠==
```html
<svg version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1098 1714.4999694824219" width="1098" height="1714.4999694824219">
	<!-- svg-source:excalidraw -->

	<defs>
		<style>
			@font-face {
				font-family: "Virgil";
				src: url("https://excalidraw.com/Virgil.woff2");
			}

			@font-face {
				font-family: "Cascadia";
				src: url("https://excalidraw.com/Cascadia.woff2");
			}
		</style>
	</defs>
	<g transform="translate(62.66668701171875 10) rotate(0 279 45.5)"><text x="0" y="17.75" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">We present a learning</text><text x="0" y="40.5" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">technique that uses neural networks to learn patterns in the</text><text x="0" y="63.25" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">input files from past fuzzing explorations to guide future fuzzing</text><text x="0" y="86" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">explorations.</text></g>
	<g transform="translate(65.3333740234375 152.66665649414062) rotate(0 314.5 34)"><text x="0" y="18.666666666666668" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">use machine learning to learn a strategy for guiding the input mutations</text><text x="0" y="41.333333333333336" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">based on previous history of executed inputs and code coverage</text><text x="0" y="64" font-family="Helvetica, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">information.</text></g>
	<g stroke-linecap="round" transform="translate(30 372.4999694824219) rotate(0 60 40)">
		<path d="M0.94 1.02 C40.71 1.63, 79.08 0.85, 121.98 -1.46 M-0.29 -0.14 C43.3 2.07, 84.44 0.17, 120.69 0.25 M121.31 1.73 C119.68 26.75, 117.8 53.65, 119.06 78.12 M119.16 0.45 C119.04 25.89, 119.73 51.9, 119.57 79.07 M121.64 80.7 C75.04 79.73, 26.61 79.42, 0.84 78.07 M121 80.05 C77.19 80.92, 33.58 81.93, -0.25 79.2 M-0.39 81.69 C-1.84 51.49, 1.18 28.27, -1.95 -0.66 M0.37 80.13 C-1.08 55.75, -0.78 31.08, -0.18 0.8" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(50 392.4999694824219) rotate(0 32.5 12.5)"><text x="32.5" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Fuzzer</text></g>
	<g stroke-linecap="round">
		<g transform="translate(157.33933549532117 409.49950548523077) rotate(0 63.2247672606199 -0.6224631719451494)">
			<path d="M0.43 -1.06 C21.75 -1.22, 106.33 -1.85, 127.46 -1.86 M-0.8 0.99 C20.49 1.06, 105.77 -0.52, 127.04 -0.65" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(157.33933549532117 409.49950548523077) rotate(0 63.2247672606199 -0.6224631719451494)">
			<path d="M97.21 11.65 C105.35 8.32, 113.82 5.37, 126.52 -1.68 M99.27 9.5 C107.69 6.35, 118.19 3.74, 126.55 -1.08" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(157.33933549532117 409.49950548523077) rotate(0 63.2247672606199 -0.6224631719451494)">
			<path d="M96.95 -8.87 C105 -6.84, 113.54 -4.44, 126.52 -1.68 M99 -11.02 C107.57 -7.28, 118.16 -3, 126.55 -1.08" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round" transform="translate(290 372.4999694824219) rotate(0 40 40)">
		<path d="M42.79 0.07 C50.29 -0.33, 59.3 3.2, 65.35 7.89 C71.39 12.58, 77.06 20.73, 79.07 28.21 C81.07 35.69, 79.95 45.34, 77.38 52.77 C74.8 60.2, 69.89 68.23, 63.6 72.79 C57.31 77.35, 47.63 80.13, 39.62 80.12 C31.62 80.11, 21.96 77.51, 15.57 72.71 C9.17 67.92, 3.39 58.93, 1.23 51.34 C-0.92 43.74, 0.04 34.27, 2.64 27.14 C5.24 20.01, 10.66 12.98, 16.81 8.56 C22.97 4.13, 34.85 1.88, 39.58 0.57 C44.31 -0.73, 43.59 0.42, 45.2 0.73 C46.82 1.03, 49.35 1.84, 49.25 2.4 M37.56 -0.71 C45.03 -1.26, 57.07 2.69, 63.49 7.29 C69.91 11.89, 73.55 19.67, 76.08 26.91 C78.61 34.15, 80.59 43.41, 78.66 50.73 C76.74 58.05, 70.6 65.88, 64.52 70.84 C58.43 75.8, 49.9 80.02, 42.13 80.47 C34.36 80.92, 24.42 77.6, 17.88 73.52 C11.34 69.45, 5.83 63.3, 2.89 56.03 C-0.05 48.77, -1.42 37.67, 0.24 29.93 C1.91 22.18, 6.76 14.56, 12.87 9.57 C18.99 4.58, 32.41 1.34, 36.9 -0.03 C41.39 -1.39, 39.34 1.29, 39.83 1.39 C40.32 1.5, 39.89 0.29, 39.85 0.6" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(308 399.9999694824219) rotate(0 22 12.5)"><text x="22" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">seed</text></g>
	<g stroke-linecap="round">
		<g transform="translate(370 412.4999694824219) rotate(0 60 0)">
			<path d="M-0.23 -0.33 C19.51 -0.63, 99.11 -1.06, 118.95 -0.81 M-1.8 -1.55 C18.25 -1.81, 100.63 0.14, 121.11 0.3" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(370 412.4999694824219) rotate(0 60 0)">
			<path d="M92.2 8.35 C101.07 4.8, 114.08 1.44, 120.1 1.07 M93.08 10.32 C99.73 7.91, 104.27 5.63, 121.22 -0.62" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(370 412.4999694824219) rotate(0 60 0)">
			<path d="M92.53 -12.17 C101.46 -8.66, 114.35 -4.96, 120.1 1.07 M93.41 -10.2 C99.96 -8.21, 104.42 -6.09, 121.22 -0.62" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round" transform="translate(490 372.4999694824219) rotate(0 80 40)">
		<path d="M80.32 -1.75 C98.27 8.81, 114.99 18.3, 160.61 41.63 M81.75 0.63 C107.81 14.57, 134.43 29.16, 159.79 41.72 M158.05 40.74 C133.79 53.49, 108.87 63.42, 81.16 81.21 M159 40.05 C144.86 50.17, 127.25 58.2, 81.69 80.04 M80.09 78.88 C65.05 71.79, 46.23 62.44, -1.6 40.73 M80.92 79.36 C56.59 67.42, 30.15 55.18, -0.17 41.35 M1.59 40.23 C16.03 34.06, 33.9 24.64, 82.01 0.43 M-0.59 40.61 C22.8 28.39, 44.38 17.31, 81.14 -0.2" stroke="#e67700" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(524.5 399.9999694824219) rotate(0 45.5 12.5)"><text x="45.5" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#e67700" text-anchor="middle" style="white-space: pre;" direction="ltr">ML Model</text></g>
	<g stroke-linecap="round">
		<g transform="translate(570 252.49996948242188) rotate(0 -10 60)">
			<path d="M-0.16 0.64 C-3.33 20.66, -16.52 101.05, -19.82 121.02 M-1.7 -0.07 C-4.94 19.55, -17.5 99.7, -20.67 119.44" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(570 252.49996948242188) rotate(0 -10 60)">
			<path d="M-25.33 90.29 C-23.13 96.97, -22.72 104.05, -22.04 118.46 M-26.22 89.31 C-25.59 95.4, -23.32 101.72, -21.07 118.7" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(570 252.49996948242188) rotate(0 -10 60)">
			<path d="M-5.06 93.5 C-7.63 99.4, -11.97 105.73, -22.04 118.46 M-5.96 92.52 C-9.61 97.8, -11.62 103.44, -21.07 118.7" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(570 232.49996948242188) rotate(0 162 25.5)"><text x="0" y="18.5" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#e67700" text-anchor="start" style="white-space: pre;" direction="ltr">evaluate/choosing mutation that</text><text x="0" y="44" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#e67700" text-anchor="start" style="white-space: pre;" direction="ltr">maximize coverage gain</text></g>
	<g transform="translate(30 492.4999694824219) rotate(0 438 35.5)"><text x="0" y="24.5" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#862e9c" text-anchor="start" style="white-space: pre;" direction="ltr">How to choose mutations (where to mutate? mutate to what?)</text><text x="0" y="60" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#862e9c" text-anchor="start" style="white-space: pre;" direction="ltr"> that MAXIMISE coverage gain?</text></g>
	<g transform="translate(30 712.4999694824219) rotate(0 32.5 12.5)"><text x="32.5" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">Fuzzer</text></g>
	<g stroke-linecap="round">
		<g transform="translate(137.33933549532117 729.4995054852308) rotate(0 63.2247672606199 -0.6224631719451565)">
			<path d="M-1.02 -0.96 C20.1 -1.07, 105.35 -0.82, 126.62 -0.84 M0.65 1.15 C21.63 0.69, 104.89 -2.61, 125.76 -2.74" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(137.33933549532117 729.4995054852308) rotate(0 63.2247672606199 -0.6224631719451565)">
			<path d="M96.23 8.5 C106.74 5.45, 114.56 0.26, 127.03 -2.89 M98.73 8.96 C104.94 5.34, 113.24 3.2, 125.46 -3.11" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(137.33933549532117 729.4995054852308) rotate(0 63.2247672606199 -0.6224631719451565)">
			<path d="M95.71 -12.01 C106.33 -9.04, 114.31 -8.21, 127.03 -2.89 M98.21 -11.56 C104.7 -9.82, 113.14 -6.59, 125.46 -3.11" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round" transform="translate(270 692.4999694824219) rotate(0 40 40)">
		<path d="M45.9 1.22 C53.25 1.85, 62.71 6.04, 68.43 11.55 C74.15 17.06, 78.88 26.4, 80.24 34.27 C81.6 42.14, 80.24 51.9, 76.59 58.78 C72.95 65.67, 65.26 71.98, 58.36 75.58 C51.46 79.19, 42.93 81.61, 35.21 80.43 C27.48 79.24, 17.67 73.95, 11.98 68.48 C6.3 63.01, 2.39 55.25, 1.09 47.61 C-0.22 39.97, 0.97 29.74, 4.14 22.63 C7.31 15.51, 13.38 8.56, 20.13 4.92 C26.88 1.28, 39.73 1.5, 44.64 0.77 C49.55 0.04, 47.99 0.27, 49.57 0.54 C51.14 0.8, 54.23 1.58, 54.08 2.37 M48.41 -0.02 C55.8 1.12, 65.04 9.18, 70.46 15.08 C75.87 20.98, 80.02 27.84, 80.91 35.37 C81.79 42.91, 79.87 53.28, 75.77 60.32 C71.67 67.35, 63.78 74.61, 56.29 77.58 C48.8 80.55, 38.49 80.12, 30.82 78.14 C23.15 76.16, 15.19 71.26, 10.28 65.7 C5.36 60.14, 2.3 52.37, 1.32 44.78 C0.34 37.19, 0.63 27.22, 4.4 20.17 C8.17 13.11, 17 5.84, 23.92 2.44 C30.84 -0.96, 41.95 -0.02, 45.91 -0.24 C49.88 -0.45, 47.32 0.59, 47.71 1.14 C48.1 1.69, 48.31 2.38, 48.26 3.09" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(288 719.9999694824219) rotate(0 22 12.5)"><text x="22" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">seed</text></g>
	<g stroke-linecap="round">
		<g transform="translate(351 732.4999694824219) rotate(0 59 0)">
			<path d="M0.41 0.12 C20.27 -0.08, 98.46 -1.02, 118.19 -1.06 M-0.84 -0.86 C18.97 -0.96, 97.46 -0.42, 117.33 -0.08" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(351 732.4999694824219) rotate(0 59 0)">
			<path d="M89.24 10.18 C93.47 8.87, 101.31 4.63, 118.77 0.8 M89.14 10.77 C100.82 5.73, 110.65 2.57, 117.69 0.25" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(351 732.4999694824219) rotate(0 59 0)">
			<path d="M89.47 -10.34 C93.68 -7.15, 101.47 -6.87, 118.77 0.8 M89.36 -9.75 C101.11 -6.8, 110.85 -1.96, 117.69 0.25" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round" transform="translate(470 692.4999694824219) rotate(0 80 40)">
		<path d="M81.31 -1.76 C107.23 12.65, 129.98 27.1, 160.88 42.79 M81.9 0.83 C110.17 14.87, 140.42 30.91, 160.32 40.68 M161.01 39.15 C137.86 51.61, 117.33 59.62, 81 80.93 M159.27 41.19 C138.53 52.54, 114.3 63.88, 81.08 80.02 M80.61 78.15 C52.74 64.97, 20.77 49.85, 1.58 39.03 M81.05 79.5 C63.72 69.78, 44.82 60.91, 0.17 40.65 M0.17 39.16 C20.81 31.63, 40.93 18.24, 80.38 0.61 M0.4 41.4 C24.51 28.31, 50.37 17.03, 81.23 -0.14" stroke="#e67700" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(504.5 719.9999694824219) rotate(0 45.5 12.5)"><text x="45.5" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#e67700" text-anchor="middle" style="white-space: pre;" direction="ltr">ML Model</text></g>
	<g stroke-linecap="round" transform="translate(10 692.4999694824219) rotate(0 60 40)">
		<path d="M-1.98 0.99 C25.32 0.84, 49.54 -0.81, 119.22 0.31 M-0.45 0.34 C34.88 -0.16, 70.18 0.45, 120.3 0.25 M118.85 -0.75 C119.89 25.15, 119.55 47.77, 118.16 78.84 M120.73 -0.38 C120.79 26.35, 121.46 51.12, 120.24 80.38 M120.71 80.87 C92.7 80.98, 64.56 79.15, 1.09 81 M120.63 80.25 C81.57 79.28, 42.93 80.72, 0.19 80.2 M-1.74 80.74 C2.22 57.63, 2.52 37.21, -1.31 -0.08 M-0.17 80.45 C-0.84 60.96, -1.34 42.75, -0.48 0.76" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g stroke-linecap="round">
		<g transform="translate(307.1535832339505 773.4010441821648) rotate(0 1.4232083830247575 59.54946265012853)">
			<path d="M1.15 -1.03 C1.58 18.74, 1.53 98.01, 1.77 118.15 M0.29 1.05 C1.15 20.98, 3.68 99.72, 3.91 119.19" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(307.1535832339505 773.4010441821648) rotate(0 1.4232083830247575 59.54946265012853)">
			<path d="M-8.73 89.45 C-6.16 97.21, -0.84 103.63, 5.6 117.56 M-6.13 91.92 C-3.67 99.78, -0.92 108.67, 3.77 118.76" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(307.1535832339505 773.4010441821648) rotate(0 1.4232083830247575 59.54946265012853)">
			<path d="M11.79 88.96 C9.57 97.03, 10.1 103.56, 5.6 117.56 M14.39 91.43 C10.74 99.28, 7.38 108.31, 3.77 118.76" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round" transform="translate(270 892.4999694824219) rotate(0 50 40)">
		<path d="M38.54 1.95 C46.97 -0.46, 58.89 -0.41, 67.62 1.8 C76.36 4.02, 85.52 9.2, 90.94 15.23 C96.35 21.27, 99.87 30.42, 100.1 38.03 C100.33 45.65, 96.97 54.47, 92.33 60.93 C87.7 67.4, 80.51 73.81, 72.28 76.81 C64.05 79.82, 51.99 80.16, 42.97 78.95 C33.96 77.74, 25.18 74.39, 18.19 69.56 C11.21 64.73, 3.79 56.83, 1.05 49.97 C-1.69 43.1, -1.34 35.12, 1.77 28.36 C4.88 21.6, 12.67 14.03, 19.71 9.4 C26.75 4.78, 39.11 2.01, 44.01 0.62 C48.92 -0.77, 49 0.42, 49.14 1.07 M30.91 4.59 C38.83 0.95, 50.71 -1.38, 59.56 -0.53 C68.41 0.33, 77.65 4.76, 84.02 9.74 C90.39 14.72, 95.85 22.14, 97.77 29.34 C99.69 36.55, 98.4 45.64, 95.53 52.98 C92.67 60.32, 87.55 68.81, 80.58 73.39 C73.61 77.97, 62.8 80.47, 53.7 80.47 C44.61 80.48, 34.12 77.14, 26 73.43 C17.89 69.71, 9.3 64.44, 5.02 58.19 C0.74 51.93, -1.09 43.49, 0.32 35.9 C1.74 28.3, 8.55 18.11, 13.5 12.61 C18.45 7.11, 27.51 4.36, 30.01 2.92 C32.51 1.48, 28.33 3.47, 28.52 3.97" stroke="#000000" stroke-width="1" fill="none"></path>
	</g>
	<g transform="translate(277.5 919.9999694824219) rotate(0 42.5 12.5)"><text x="42.5" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="middle" style="white-space: pre;" direction="ltr">mutation</text></g>
	<g stroke-linecap="round">
		<g transform="translate(370 892.4999694824219) rotate(0 70 -60)">
			<path d="M0.07 0.42 C23.55 -19.26, 117.61 -98.93, 141.13 -119.14 M-1.36 -0.4 C22.04 -20.33, 117.04 -100.88, 140.77 -120.81" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(370 892.4999694824219) rotate(0 70 -60)">
			<path d="M126.56 -92.91 C132.29 -101.21, 134.24 -111.49, 139.51 -121.92 M126.41 -93.96 C130.07 -99.99, 133.5 -106.64, 140.77 -120.33" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(370 892.4999694824219) rotate(0 70 -60)">
			<path d="M113.32 -108.58 C123.16 -111.86, 129.36 -117.11, 139.51 -121.92 M113.17 -109.63 C119.54 -112.22, 125.83 -115.48, 140.77 -120.33" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round">
		<g transform="translate(537.0416763845271 688.9791312901583) rotate(0 -100.93747942761513 -34.90630606255962)">
			<path d="M0.16 0.37 C-15.74 -10.97, -62.75 -68.64, -96.59 -69 C-130.44 -69.36, -185.51 -12.69, -202.9 -1.79 M-1.21 -0.49 C-17.17 -12.08, -64.35 -70.15, -97.57 -70.69 C-130.79 -71.24, -183.45 -15.04, -200.54 -3.77" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(537.0416763845271 688.9791312901583) rotate(0 -100.93747942761513 -34.90630606255962)">
			<path d="M-184.8 -29.77 C-187.32 -26.13, -190.58 -17.55, -202.39 -3.23 M-185.71 -29.97 C-189.29 -23.1, -194.87 -14.58, -199.7 -4.36" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(537.0416763845271 688.9791312901583) rotate(0 -100.93747942761513 -34.90630606255962)">
			<path d="M-171.69 -13.98 C-176.72 -13.52, -182.63 -8.12, -202.39 -3.23 M-172.59 -14.19 C-179.87 -11.83, -189.2 -7.84, -199.7 -4.36" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g stroke-linecap="round">
		<g transform="translate(630.4660579376668 739.0305613937354) rotate(0 141.46704075503442 -2.766249378106295)">
			<path d="M0.65 -1.1 C47.78 -2.05, 236.06 -4.19, 282.93 -4.8 M-0.47 0.94 C46.49 -0.44, 234.53 -5.27, 281.98 -6.53" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(630.4660579376668 739.0305613937354) rotate(0 141.46704075503442 -2.766249378106295)">
			<path d="M252.24 4.46 C263.98 -0.9, 272.87 -1.1, 282.17 -5.19 M254.65 3.88 C261.96 0.99, 271.47 -2.99, 281.97 -5.55" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(630.4660579376668 739.0305613937354) rotate(0 141.46704075503442 -2.766249378106295)">
			<path d="M251.7 -16.05 C263.78 -14.97, 272.83 -8.72, 282.17 -5.19 M254.12 -16.63 C261.45 -13.29, 271.13 -11.04, 281.97 -5.55" stroke="#e67700" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(930 712.4999694824219) rotate(0 79 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">Target Program</text></g>
	<g transform="translate(670 752.4999694824219) rotate(0 92.5 10)"><text x="0" y="14" font-family="Virgil, Segoe UI Emoji" font-size="16px" fill="#e67700" text-anchor="start" style="white-space: pre;" direction="ltr">Mutates good positions</text></g>
	<g transform="translate(490 612.4999694824219) rotate(0 88 10)"><text x="0" y="14" font-family="Virgil, Segoe UI Emoji" font-size="16px" fill="#e67700" text-anchor="start" style="white-space: pre;" direction="ltr">mutates bad positions</text></g>
	<g transform="translate(50 1012.4999694824219) rotate(0 377.5 17.5)"><text x="0" y="25" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#862e9c" text-anchor="start" style="white-space: pre;" direction="ltr">How to assess input positions, where shall we mutate?</text></g>
	<g transform="translate(70 1092.4999694824219) rotate(0 316.5 12)"><text x="0" y="20" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">f(mutation|x) = Gain(coverage(x), coverage(mutate(x)))</text></g>
	<g stroke-linecap="round">
		<g transform="translate(299.83644559917946 1251.4999694824219) rotate(0 -4.385924914589694 -59.49999999999994)">
			<path d="M-0.96 -1.02 C-2.48 -21.15, -8.08 -100.53, -9.58 -120.01 M0.74 1.06 C-0.36 -19.03, -5.89 -98.88, -7.3 -119.01" stroke="#087f5b" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(299.83644559917946 1251.4999694824219) rotate(0 -4.385924914589694 -59.49999999999994)">
			<path d="M3.19 -92.95 C-0.18 -100.86, 0.13 -105.42, -6.59 -120.85 M5.23 -91.79 C0.86 -99.18, -1.65 -106.9, -7.19 -118.19" stroke="#087f5b" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(299.83644559917946 1251.4999694824219) rotate(0 -4.385924914589694 -59.49999999999994)">
			<path d="M-17.29 -91.53 C-15.3 -99.65, -9.63 -104.58, -6.59 -120.85 M-15.24 -90.37 C-13.53 -98.29, -9.94 -106.44, -7.19 -118.19" stroke="#087f5b" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(290 1252.4999694824219) rotate(0 148.5 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#862e9c" text-anchor="start" style="white-space: pre;" direction="ltr">this is what we want to learn</text></g>
	<g transform="translate(50 1332.4999694824219) rotate(0 250.5 17.5)"><text x="0" y="25" font-family="Virgil, Segoe UI Emoji" font-size="28px" fill="#862e9c" text-anchor="start" style="white-space: pre;" direction="ltr">How to define the gain of coverage?</text></g>
	<g transform="translate(70 1392.4999694824219) rotate(0 304.5 12)"><text x="0" y="20" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#087f5b" text-anchor="start" style="white-space: pre;" direction="ltr">Gain(b, b') = sum of new ones in the coverage bitmap</text></g>
	<g transform="translate(30 1512.4999694824219) rotate(0 340 96)"><text x="0" y="20" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">The goal of</text><text x="0" y="44" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">augmented fuzzing is to improve the hit-rate of mutations.</text><text x="0" y="68" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">Using the annotated seed provided by the model, mutations</text><text x="0" y="92" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">that are unlikely to give input gain are avoided. </text><text x="0" y="116" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr"></text><text x="0" y="140" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">We used a</text><text x="0" y="164" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">highly permissive veto approach to reject mutations which</text><text x="0" y="188" font-family="Cascadia, Segoe UI Emoji" font-size="20px" fill="#000000" text-anchor="start" style="white-space: pre;" direction="ltr">target no useful locations.</text></g>
	<g stroke-linecap="round">
		<g transform="translate(869 1629.8348337087991) rotate(0 -78.99999999999997 13.401359152309965)">
			<path d="M-0.04 0.8 C-26.52 5.22, -131.9 22.57, -158.12 26.87 M-1.52 0.17 C-28.24 4.78, -133.08 23.84, -159.13 28.44" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(869 1629.8348337087991) rotate(0 -78.99999999999997 13.401359152309965)">
			<path d="M-131.86 13.17 C-142.52 19.92, -152.5 23.52, -158.34 26.76 M-133.55 13.26 C-142.36 18.93, -148.88 22.89, -158.47 28.32" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
		<g transform="translate(869 1629.8348337087991) rotate(0 -78.99999999999997 13.401359152309965)">
			<path d="M-128.24 33.36 C-140.27 32.73, -151.57 28.96, -158.34 26.76 M-129.92 33.45 C-139.79 32.67, -147.46 30.21, -158.47 28.32" stroke="#000000" stroke-width="1" fill="none"></path>
		</g>
	</g>
	<g transform="translate(870 1612.4999694824219) rotate(0 70 12.5)"><text x="0" y="18" font-family="Virgil, Segoe UI Emoji" font-size="20px" fill="#862e9c" text-anchor="start" style="white-space: pre;" direction="ltr">why this works</text></g>
</svg>
```
%%