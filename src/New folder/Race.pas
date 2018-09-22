program Race;
uses SwinGame, sgTypes, sysUtils;

const
	WORLD = 'world.png';
	SKY = 'sky.png';
	GAUGE = 'gauge.png';
	NEEDLE = 'needle.png';

	BENZ = 'benz.png';
	AUDIBLACK = 'audi.png';
	AUDISILVER = 'audi-silver.png';
	AUDIBLUE = 'audi-blue.png';
	AUDIGOLD = 'audi-gold.png';
	AUDIRED = 'audi-red.png';

	TREEBMP = 'tree.png';
	OBSBMP = 'obs.png';

	CRASHED = 'crashed.png';

	HORIZON = 200;

	UPSIDEDOWNRIGHT = 180; 
	UPSIDEDOWNLEFT = -180;

	ROADLIMITRIGHT = 600;
	ROADLIMITLEFT = 100;

type
	Position = record
		X : Single;
		Y : Single;
	end;

	Speed = record
		spdXpos: Single;
		spdXneg: Single;
		spdY: Single;
	end;

	Car = record
		pos : Position;
		angle: Integer;
		maker: String;
	end;

	OtherCars = record
		pos : Position;
		spd : Speed;
		maker: String;
		line: String;
	end;

	GameData = record
		PlaySpeed: Speed;
		CarObject: Car;
		Line: Position;
		Tree: Position;
		Traffic: Array [0..2] of OtherCars;
		paused: Boolean;
	end;


//if Rnd(2) = 1 then;
//Rnd(blabla);

procedure LoadResources();
begin
 	LoadBitmapNamed('world', world);
 	LoadBitmapNamed('sky', sky);
 	LoadBitmapNamed('gauge', gauge);
 	LoadBitmapNamed('needle', needle);

 	LoadBitmapNamed('audi', audiblack);
 	LoadBitmapNamed('audisilver', audisilver);
 	LoadBitmapNamed('audiblue', audiblue);
 	LoadBitmapNamed('audigold', audigold);
 	LoadBitmapNamed('audired', audired);

 	LoadBitmapNamed('benz', benz);

 	LoadBitmapNamed('crashed', crashed);

end;

procedure PauseGame();
begin
	WriteLn('Game Paused...');
	WriteLn('Press any key to continue..');
	ReadLn();

end;

procedure IncSpeed(var PlaySpeed:Speed);
begin
	if playspeed.spdY < 0.5 then PlaySpeed.spdy := playspeed.spdY + 0.08;
	if playspeed.spdY < 25 then playspeed.spdy := playspeed.spdy * ((playspeed.spdy + 0.08) / playspeed.spdY);
	playspeed.spdXpos := playspeed.spdY / 2;
	playspeed.spdXneg := playspeed.spdXpos * (- 1);

end;

procedure DecSpeed(var PlaySpeed:Speed);
begin
	if playspeed.spdY > 0 then
		begin
 		playspeed.spdY := playspeed.spdY / ((playspeed.spdy + 0.2) / playspeed.spdY);
 		playspeed.spdXpos := playspeed.spdY / 2;
 		PlaySpeed.spdXneg := playspeed.spdXpos * - 1;
 		end;
	
end;

procedure DrawLines(var game:GameData);
begin
 	FillRectangle(colorYellow, game.Line.X, game.Line.Y, 10, 50);
 	FillRectangle(colorYellow, game.Line.X, game.Line.Y + 100, 10, 50);
 	FillRectangle(colorYellow, game.Line.X, game.Line.Y + 200, 10, 50);
 	FillRectangle(colorYellow, game.Line.X, game.Line.Y + 300, 10, 50);
 	FillRectangle(colorYellow, game.Line.X, game.Line.Y + 400, 10, 50);
 	FillRectangle(colorYellow, game.Line.X, game.Line.Y + 500, 10, 50);

 	game.Line.Y := game.Line.Y + game.PlaySpeed.spdY;
 	if game.Line.Y > HORIZON then game.Line.Y := 0;

end;

procedure DrawTree(var game:GameData);
begin
	DrawBitmap(TREEBMP, game.Tree.X, game.Tree.Y);

	game.Tree.Y := game.Tree.Y + game.playspeed.spdY;

	if game.Tree.X < -80 then
		begin
			game.Tree.X := 500;
			game.Tree.Y := 100;
		end;

	if game.Tree.X > 800 then
		begin
			game.Tree.X := 200;
			game.Tree.Y := 100;
		end;

	if game.Tree.X < 400 then game.Tree.X := game.Tree.X + game.playspeed.spdXneg;
	if game.Tree.X > 400 then game.Tree.X := game.Tree.X + game.playspeed.spdXpos;

end;

procedure ResetGame(var game:GameData);
begin
	game.PlaySpeed.spdY := 0;
	game.playspeed.spdXpos := 0;
	game.playspeed.spdXneg := 0;
	game.CarObject.pos.X := 350;
	game.CarObject.angle := 0;
end;

procedure DrawGauge(var PlaySpeed:Speed);
var
	gaugeAngle : Single;
begin
	DrawBitmap('gauge',250,50);
	gaugeAngle := ((playspeed.spdY * 7.2) + 90);
	DrawBitmap('needle', 383, 45, OptionRotateBmp(gaugeAngle));
	FillCircle(colorBlack, 400, 185, 15);
end;

procedure DrawCar(var CarObject:Car);
begin
 	DrawBitmap(CarObject.maker, CarObject.pos.X, CarObject.pos.Y, OptionRotateBmp(CarObject.angle));
end;

procedure UpdateCars(var ot:OtherCars; var game:GameData);
begin
	ot.spd.spdY := 8 - game.PlaySpeed.spdY;
	ot.pos.Y := ot.pos.Y - ot.spd.spdY;
	ot.spd.spdXpos := ot.spd.spdY / 8;
	ot.spd.spdXneg := ot.spd.spdXpos * - 1;

	if ot.line = 'left' then ot.pos.X := ot.pos.x + ot.spd.spdXpos;
	if ot.line = 'right' then ot.pos.X := ot.pos.x + ot.spd.spdXneg;
end;

procedure UpdateTraffic(var Traffic: Array of OtherCars; var game:GameData);
var
	i: Integer;
begin
	for i := 0 to High(Traffic) do
		begin
			UpdateCars(Traffic[i], game);
		end;
end;

procedure DrawCars(var ot:OtherCars);
begin
	DrawBitmap(ot.maker, ot.pos.X, ot.pos.Y);
end;

procedure DrawTraffic(var Traffic: Array of OtherCars);
var
	i: Integer;
begin
	for i := 0 to High(Traffic) do
		begin
			DrawCars(Traffic[i]);
		end;
end;

//procedure InitialiseCars(var ot: OtherCars);
//begin
//
//end;

procedure InitialiseTraffic(var Traffic: Array of OtherCars);
var
	i: Integer;
begin
	Traffic[0].pos.X := 275;
	Traffic[0].pos.Y := 350;
	Traffic[0].maker := AUDIBLUE;
	Traffic[0].line := 'left';
	Traffic[1].pos.X := 425;
	Traffic[1].pos.Y := 350;
	Traffic[1].maker := AUDIGOLD;
	Traffic[1].line := 'right';
	Traffic[2].pos.X := 250;
	Traffic[2].pos.Y := 500;
	Traffic[2].maker := AUDIRED;
	Traffic[2].line := 'left';
//	for i := 0 to High(Traffic) do
//		begin
//			InitialiseCars(Traffic[i]);
//		end;

end;

procedure Crash(var game:GameData);
var
	i: Integer;
begin
	if (game.CarObject.pos.X > ROADLIMITRIGHT) then i := 1;
	if (game.CarObject.pos.X < ROADLIMITLEFT) then i := -1;

	if (game.CarObject.pos.X > ROADLIMITRIGHT) or (game.CarObject.pos.X < ROADLIMITLEFT) then
		begin
			while (game.CarObject.angle = UPSIDEDOWNRIGHT + i) do
			DrawCar(game.CarObject);
			DrawBitmap(crashed, 100, 0);
			game.CarObject.pos.X := game.CarObject.pos.X + (2 * i);
			game.CarObject.angle := game.CarObject.angle + (2 * i);
			DecSpeed(game.playspeed);
			if (game.CarObject.angle > UPSIDEDOWNRIGHT) or (game.CarObject.angle < UPSIDEDOWNLEFT) then delay(1000);
			if (game.CarObject.angle > UPSIDEDOWNRIGHT) or (game.CarObject.angle < UPSIDEDOWNLEFT) then ResetGame(game);
		end;
end;

procedure SelectCar (var CarObject:Car);
begin
	ProcessEvents();
	begin;
	if KeyDown(Keypad1) then CarObject.maker := AUDIBLACK;
	if KeyDown(Keypad2) then CarObject.maker := AUDISILVER;
	if KeyDown(Keypad3) then CarObject.maker := AUDIBLUE;
	if KeyDown(Keypad4) then CarObject.maker := AUDIGOLD;
	if KeyDown(Keypad5) then CarObject.maker := AUDIRED;

	if KeyDown(Keypad7) then CarObject.maker := AUDIBLACK;
 	if KeyDown(Keypad8) then CarObject.maker := BENZ;
 	end;
end;

procedure GiveMeData(var game:GameData);
begin
	WriteLn('Car X POSN : ', FloattoStr(game.CarObject.pos.X));
	WriteLn('Speed : ', FloatToStr(game.playspeed.spdY));
	WriteLn('ot Xpos : ', FloatToStr(game.Traffic[1].spd.spdXpos));
	WriteLn('ot Xneg: ', FloatToStr(game.Traffic[1].spd.spdXneg));
	WriteLn('ot Y : ', FloatToStr(game.Traffic[1].pos.Y));
	WriteLn('ot spd : ', FloatToStr(game.Traffic[1].spd.spdY));

end;

procedure UserControls(var game:GameData);
begin
 	ProcessEvents();
 	if	(game.CarObject.pos.X > ROADLIMITLEFT - 2) and
 		(game.CarObject.pos.X < ROADLIMITRIGHT + 2) then
	begin
		game.CarObject.angle := 0;
	 	if KeyDown(UpKey) then IncSpeed(game.playspeed);
		if KeyDown(DownKey) then DecSpeed(game.playspeed);
		if KeyDown(RightKey) then game.CarObject.pos.X := game.CarObject.pos.X + (game.playspeed.spdY / 2);
		if KeyDown(RightKey) then game.CarObject.angle := + 10;
		if KeyDown(LeftKey) then game.CarObject.pos.X := game.CarObject.pos.X - (game.playspeed.spdY / 2);
		if KeyDown(LeftKey) then game.CarObject.angle := - 10;
		if KeyDown(ZKey) then GiveMeData(game);
	end;

 end;

procedure DrawGame(var game: GameData);
begin
	DrawCar(game.CarObject);
end;

procedure InitialiseWorld(var game: GameData);
begin
	game.PlaySpeed.spdY := 0;
	game.PlaySpeed.spdXpos := 0;
	game.PlaySpeed.spdXneg := 0;
	game.CarObject.pos.X := 350;
	game.CarObject.pos.Y := 475;
	game.CarObject.angle := 0;
	game.CarObject.maker := AUDIBLACK;
	game.Line.X := 395;
	game.Line.Y := 1;
	game.Tree.X := 200;
	game.Tree.Y := 100;
	game.paused := false;

	OpenGraphicsWindow('Race', 800, 600);
 	ClearScreen(colorWhite);
  	//ShowSwinGameSplashScreen();
 	LoadResources();


end;

procedure Main();
var
	game:GameData;
begin
	InitialiseWorld(game);
	InitialiseTraffic(game.Traffic);

	repeat
		UserControls(game);
 		begin
	 		RefreshScreen(60);
	 		DrawBitmap('world', 0, 0);
	 		DrawLines(game);
	 		DrawTree(game);
	 		DrawCar(game.CarObject);
	 		SelectCar(game.CarObject);
	 		DrawTraffic(game.Traffic);
			UpdateTraffic(game.Traffic, game);
	 		DrawBitmap('sky', 0, 0);
	 		DrawGauge(game.playspeed);
	 		Crash(game);
 		end;
	until WindowCloseRequested();
end;

begin
	Main();
end.
