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
		id : Integer;
		pos : Position;
		angle: Integer;
		maker: String;
		line: String;
	end;

	GameData = record
		PlaySpeed: Speed;
		Car[0]: Car;
		Line: Position;
		Tree: Position;
		Racers: Array [0..12] of Car;
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
	game.Car[0].pos.X := 350;
	game.Car[0].angle := 0;
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

procedure DrawCar(var Car[0]:Car);
begin
 	DrawBitmap(Car[0].maker, Car[0].pos.X, Car[0].pos.Y, OptionRotateBmp(Car[0].angle));
end;

procedure UpdateCars(var racer: Car; var game:GameData);
begin
	racer.spd.spdY := 8 - game.PlaySpeed.spdY;
	racer.pos.Y := racer.pos.Y - racer.spd.spdY;
	racer.spd.spdXpos := racer.spd.spdY / 8;
	racer.spd.spdXneg := racer.spd.spdXpos * - 1;

	if racer.line = 'left' then racer.pos.X := racer..pos.x + racer..spd.spdXpos;
	if racer.line = 'right' then racer.pos.X := racer.pos.x + racer..spd.spdXneg;
end;

procedure UpdateRacers(var Racers: Array of Car; var game:GameData);
var
	i: Integer;
begin
	for i := 1 to High(Racers) do
		begin
			UpdateCars(Racers[i], game);
		end;
end;

procedure InitialiseCars(var racer: Car);
begin
	if racer.carId = 1 or 3 or 5 or 6 or 8 or 11 or 12 then
	begin
		racer.line := 'left';
		racer.pos.X : 300 + (racer.carId * 25);
		racer.pos.y : 200 - (racer.carId * 150);
		racer.maker: GetCarColour(racer.carId);
		DrawBitmap(racer.maker, racer.pos.X, racer.pos.Y)
	end;
	if racer.carId = 2 or 4 or 7 or 9 or 10 then
	begin
		racer.line := 'right';
		racer.pos.X : 400 - (racer.carId * 25);
		racer.pos.y : 200 - (racer.carId * 150);
		racer.maker: GetCarColour(racer.carId);
		DrawBitmap(racer.maker, racer.pos.X, racer.pos.Y)
	end;
end;

procedure InitialiseRacers(var Racers: Array of Car);
var
	i: Integer;
begin
	for i := 1 to High(Racers) do
		begin
			Racers[i].carId := i;
			InitialiseCars(Racers[i]);
		end;
end;

procedure Crash(var game:GameData);
var
	i: Integer;
begin
	if (game.Car[0].pos.X > ROADLIMITRIGHT) then i := 1;
	if (game.Car[0].pos.X < ROADLIMITLEFT) then i := -1;

	if (game.Car[0].pos.X > ROADLIMITRIGHT) or (game.Car[0].pos.X < ROADLIMITLEFT) then
		begin
			while (game.Car[0].angle = UPSIDEDOWNRIGHT + i) do
			DrawCar(game.Car[0]);
			DrawBitmap(crashed, 100, 0);
			game.Car[0].pos.X := game.Car[0].pos.X + (2 * i);
			game.Car[0].angle := game.Car[0].angle + (2 * i);
			DecSpeed(game.playspeed);
			if (game.Car[0].angle > UPSIDEDOWNRIGHT) or (game.Car[0].angle < UPSIDEDOWNLEFT) then delay(1000);
			if (game.Car[0].angle > UPSIDEDOWNRIGHT) or (game.Car[0].angle < UPSIDEDOWNLEFT) then ResetGame(game);
		end;
end;

procedure SelectCar (var Car[0]:Car);
begin
	ProcessEvents();
	begin;
	if KeyDown(Keypad1) then Car[0].maker := AUDIBLACK;
	if KeyDown(Keypad2) then Car[0].maker := AUDISILVER;
	if KeyDown(Keypad3) then Car[0].maker := AUDIBLUE;
	if KeyDown(Keypad4) then Car[0].maker := AUDIGOLD;
	if KeyDown(Keypad5) then Car[0].maker := AUDIRED;
	if KeyDown(Keypad7) then Car[0].maker := AUDIBLACK;
 	if KeyDown(Keypad8) then Car[0].maker := BENZ;
 	end;
end;

procedure GiveMeData(var game:GameData);
begin
	WriteLn('Car X POSN : ', FloattoStr(game.Car[0].pos.X));
	WriteLn('Speed : ', FloatToStr(game.playspeed.spdY));
	WriteLn('Racer1 Xpos : ', FloatToStr(game.Racers[1].spd.spdXpos));
	WriteLn('Racer1 Xneg: ', FloatToStr(game.Racers[1].spd.spdXneg));
	WriteLn('Racer1 Y : ', FloatToStr(game.Racers[1].pos.Y));
	WriteLn('Racer1 spd : ', FloatToStr(game.Racers[1].spd.spdY));

end;

procedure UserControls(var game:GameData);
begin
 	ProcessEvents();
 	if	(game.Car[0].pos.X > ROADLIMITLEFT - 2) and
 		(game.Car[0].pos.X < ROADLIMITRIGHT + 2) then
	begin
		game.Car[0].angle := 0;
	 	if KeyDown(UpKey) then IncSpeed(game.playspeed);
		if KeyDown(DownKey) then DecSpeed(game.playspeed);
		if KeyDown(RightKey) then game.Car[0].pos.X := game.Car[0].pos.X + (game.playspeed.spdY / 2);
		if KeyDown(RightKey) then game.Car[0].angle := + 10;
		if KeyDown(LeftKey) then game.Car[0].pos.X := game.Car[0].pos.X - (game.playspeed.spdY / 2);
		if KeyDown(LeftKey) then game.Car[0].angle := - 10;
		if KeyDown(ZKey) then GiveMeData(game);
	end;

 end;

procedure InitialiseWorld(var game: GameData);
begin
	game.PlaySpeed.spdY := 0;
	game.PlaySpeed.spdXpos := 0;
	game.PlaySpeed.spdXneg := 0;
	game.Car[0].pos.X := 350;
	game.Car[0].pos.Y := 475;
	game.Car[0].angle := 0;
	game.Car[0].maker := AUDIBLACK;
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
	InitialiseRacers(game.Racers);

	repeat
		UserControls(game);
 		begin
	 		RefreshScreen(60);
	 		DrawBitmap('world', 0, 0);
	 		DrawLines(game);
	 		DrawTree(game);
	 		DrawCar(game.Car[0]);
	 		SelectCar(game.Car[0]);
	 		DrawRacers(game.Racers);
			UpdateRacers(game.Racers, game);
	 		DrawBitmap('sky', 0, 0);
	 		DrawGauge(game.playspeed);
	 		Crash(game);
 		end;
	until WindowCloseRequested();
end;

begin
	Main();
end.
