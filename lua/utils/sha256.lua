--
-- SHA-256 secure hash computation, and HMAC-SHA256 signature computation
--
-- Copyright 2017 Jqqqi
--
local sha256 = { }

local MOD = 2 ^ 32
local MODM = MOD - 1
 
local function memoize(f)
	local mt = { }
	local t = setmetatable( { }, mt)
	function mt:__index(k)
		local v = f(k)
		t[k] = v
		return v
	end
	return t
end
 
local function make_bitop_uncached(t, m)
	local function bitop(a, b)
		local res, p = 0, 1
		while a ~= 0 and b ~= 0 do
			local am, bm = a % m, b % m
			res = res + t[am][bm] * p
			a =(a - am) / m
			b =(b - bm) / m
			p = p * m
		end
		res = res +(a + b) * p
		return res
	end
	return bitop
end
 
local function make_bitop(t)
	local op1 = make_bitop_uncached(t, 2 ^ 1)
	local op2 = memoize( function(a) return memoize( function(b) return op1(a, b) end) end)
	return make_bitop_uncached(op2, 2 ^(t.n or 1))
end
 
local bxor1 = make_bitop( { [0] = { [0] = 0, [1] = 1 }, [1] = { [0] = 1, [1] = 0 }, n = 4 })
 
local function bxor(a, b, c, ...)
	local z = nil
	if b then
		a = a % MOD
		b = b % MOD
		z = bxor1(a, b)
		if c then z = bxor(z, c, ...) end
		return z
	elseif a then
		return a % MOD
	else
		return 0
	end
end
 
local function band(a, b, c, ...)
	local z
	if b then
		a = a % MOD
		b = b % MOD
		z =((a + b) - bxor1(a, b)) / 2
		if c then z = bit32_band(z, c, ...) end
		return z
	elseif a then
		return a % MOD
	else
		return MODM
	end
end
 
local function bnot(x) return(-1 - x) % MOD end
 
local function rshift1(a, disp)
	if disp < 0 then return lshift(a, - disp) end
	return math.floor(a % 2 ^ 32 / 2 ^ disp)
end
 
local function rshift(x, disp)
	if disp > 31 or disp < -31 then return 0 end
	return rshift1(x % MOD, disp)
end
 
local function lshift(a, disp)
	if disp < 0 then return rshift(a, - disp) end
	return(a * 2 ^ disp) % 2 ^ 32
end
 
local function rrotate(x, disp)
	x = x % MOD
	disp = disp % 32
	local low = band(x, 2 ^ disp - 1)
	return rshift(x, disp) + lshift(low, 32 - disp)
end
 
local k = {
	0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,
	0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
	0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,
	0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
	0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,
	0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
	0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,
	0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
	0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,
	0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
	0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,
	0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
	0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,
	0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
	0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,
	0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2,
}
 
local function str2hexa(s)
	return(string.gsub(s, ".", function(c) return string.format("%02x", string.byte(c)) end))
end
 
local function num2s(l, n)
	local s = ""
	for i = 1, n do
		local rem = l % 256
		s = string.char(rem) .. s
		l =(l - rem) / 256
	end
	return s
end
 
local function s232num(s, i)
	local n = 0
	for i = i, i + 3 do n = n * 256 + string.byte(s, i) end
	return n
end
 
local function preproc(msg, len)
	local extra = 64 - ((len + 9) % 64)
	if extra == 64 then extra = 0 end
	len = num2s(8 * len, 8)
	msg = msg .. "\128" .. string.rep("\0", extra) .. len
	assert(#msg % 64 == 0)
	return msg
end
 
local function initH256(H)
	H[1] = 0x6a09e667
	H[2] = 0xbb67ae85
	H[3] = 0x3c6ef372
	H[4] = 0xa54ff53a
	H[5] = 0x510e527f
	H[6] = 0x9b05688c
	H[7] = 0x1f83d9ab
	H[8] = 0x5be0cd19
	return H
end
 
local function digestblock(msg, i, H)
	local w = { }
	for j = 1, 16 do w[j] = s232num(msg, i +(j - 1) * 4) end
	for j = 17, 64 do
		local v = w[j - 15]
		local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
		v = w[j - 2]
		w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
	end

	local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
	for i = 1, 64 do
		local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
		local maj = bxor(band(a, b), band(a, c), band(b, c))
		local t2 = s0 + maj
		local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
		local ch = bxor(band(e, f), band(bnot(e), g))
		local t1 = h + s1 + ch + k[i] + w[i]
		h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
	end

	H[1] = band(H[1] + a)
	H[2] = band(H[2] + b)
	H[3] = band(H[3] + c)
	H[4] = band(H[4] + d)
	H[5] = band(H[5] + e)
	H[6] = band(H[6] + f)
	H[7] = band(H[7] + g)
	H[8] = band(H[8] + h)
end

local function hex_to_binary(hex)
	return hex:gsub('..', function(hexval)
		return string.char(tonumber(hexval, 16))
	end )
end

local blocksize = 64 -- 512 bits

local xor_with_0x5c = {
	[string.char(0)] = string.char(92),
	[string.char(1)] = string.char(93),
	[string.char(2)] = string.char(94),
	[string.char(3)] = string.char(95),
	[string.char(4)] = string.char(88),
	[string.char(5)] = string.char(89),
	[string.char(6)] = string.char(90),
	[string.char(7)] = string.char(91),
	[string.char(8)] = string.char(84),
	[string.char(9)] = string.char(85),
	[string.char(10)] = string.char(86),
	[string.char(11)] = string.char(87),
	[string.char(12)] = string.char(80),
	[string.char(13)] = string.char(81),
	[string.char(14)] = string.char(82),
	[string.char(15)] = string.char(83),
	[string.char(16)] = string.char(76),
	[string.char(17)] = string.char(77),
	[string.char(18)] = string.char(78),
	[string.char(19)] = string.char(79),
	[string.char(20)] = string.char(72),
	[string.char(21)] = string.char(73),
	[string.char(22)] = string.char(74),
	[string.char(23)] = string.char(75),
	[string.char(24)] = string.char(68),
	[string.char(25)] = string.char(69),
	[string.char(26)] = string.char(70),
	[string.char(27)] = string.char(71),
	[string.char(28)] = string.char(64),
	[string.char(29)] = string.char(65),
	[string.char(30)] = string.char(66),
	[string.char(31)] = string.char(67),
	[string.char(32)] = string.char(124),
	[string.char(33)] = string.char(125),
	[string.char(34)] = string.char(126),
	[string.char(35)] = string.char(127),
	[string.char(36)] = string.char(120),
	[string.char(37)] = string.char(121),
	[string.char(38)] = string.char(122),
	[string.char(39)] = string.char(123),
	[string.char(40)] = string.char(116),
	[string.char(41)] = string.char(117),
	[string.char(42)] = string.char(118),
	[string.char(43)] = string.char(119),
	[string.char(44)] = string.char(112),
	[string.char(45)] = string.char(113),
	[string.char(46)] = string.char(114),
	[string.char(47)] = string.char(115),
	[string.char(48)] = string.char(108),
	[string.char(49)] = string.char(109),
	[string.char(50)] = string.char(110),
	[string.char(51)] = string.char(111),
	[string.char(52)] = string.char(104),
	[string.char(53)] = string.char(105),
	[string.char(54)] = string.char(106),
	[string.char(55)] = string.char(107),
	[string.char(56)] = string.char(100),
	[string.char(57)] = string.char(101),
	[string.char(58)] = string.char(102),
	[string.char(59)] = string.char(103),
	[string.char(60)] = string.char(96),
	[string.char(61)] = string.char(97),
	[string.char(62)] = string.char(98),
	[string.char(63)] = string.char(99),
	[string.char(64)] = string.char(28),
	[string.char(65)] = string.char(29),
	[string.char(66)] = string.char(30),
	[string.char(67)] = string.char(31),
	[string.char(68)] = string.char(24),
	[string.char(69)] = string.char(25),
	[string.char(70)] = string.char(26),
	[string.char(71)] = string.char(27),
	[string.char(72)] = string.char(20),
	[string.char(73)] = string.char(21),
	[string.char(74)] = string.char(22),
	[string.char(75)] = string.char(23),
	[string.char(76)] = string.char(16),
	[string.char(77)] = string.char(17),
	[string.char(78)] = string.char(18),
	[string.char(79)] = string.char(19),
	[string.char(80)] = string.char(12),
	[string.char(81)] = string.char(13),
	[string.char(82)] = string.char(14),
	[string.char(83)] = string.char(15),
	[string.char(84)] = string.char(8),
	[string.char(85)] = string.char(9),
	[string.char(86)] = string.char(10),
	[string.char(87)] = string.char(11),
	[string.char(88)] = string.char(4),
	[string.char(89)] = string.char(5),
	[string.char(90)] = string.char(6),
	[string.char(91)] = string.char(7),
	[string.char(92)] = string.char(0),
	[string.char(93)] = string.char(1),
	[string.char(94)] = string.char(2),
	[string.char(95)] = string.char(3),
	[string.char(96)] = string.char(60),
	[string.char(97)] = string.char(61),
	[string.char(98)] = string.char(62),
	[string.char(99)] = string.char(63),
	[string.char(100)] = string.char(56),
	[string.char(101)] = string.char(57),
	[string.char(102)] = string.char(58),
	[string.char(103)] = string.char(59),
	[string.char(104)] = string.char(52),
	[string.char(105)] = string.char(53),
	[string.char(106)] = string.char(54),
	[string.char(107)] = string.char(55),
	[string.char(108)] = string.char(48),
	[string.char(109)] = string.char(49),
	[string.char(110)] = string.char(50),
	[string.char(111)] = string.char(51),
	[string.char(112)] = string.char(44),
	[string.char(113)] = string.char(45),
	[string.char(114)] = string.char(46),
	[string.char(115)] = string.char(47),
	[string.char(116)] = string.char(40),
	[string.char(117)] = string.char(41),
	[string.char(118)] = string.char(42),
	[string.char(119)] = string.char(43),
	[string.char(120)] = string.char(36),
	[string.char(121)] = string.char(37),
	[string.char(122)] = string.char(38),
	[string.char(123)] = string.char(39),
	[string.char(124)] = string.char(32),
	[string.char(125)] = string.char(33),
	[string.char(126)] = string.char(34),
	[string.char(127)] = string.char(35),
	[string.char(128)] = string.char(220),
	[string.char(129)] = string.char(221),
	[string.char(130)] = string.char(222),
	[string.char(131)] = string.char(223),
	[string.char(132)] = string.char(216),
	[string.char(133)] = string.char(217),
	[string.char(134)] = string.char(218),
	[string.char(135)] = string.char(219),
	[string.char(136)] = string.char(212),
	[string.char(137)] = string.char(213),
	[string.char(138)] = string.char(214),
	[string.char(139)] = string.char(215),
	[string.char(140)] = string.char(208),
	[string.char(141)] = string.char(209),
	[string.char(142)] = string.char(210),
	[string.char(143)] = string.char(211),
	[string.char(144)] = string.char(204),
	[string.char(145)] = string.char(205),
	[string.char(146)] = string.char(206),
	[string.char(147)] = string.char(207),
	[string.char(148)] = string.char(200),
	[string.char(149)] = string.char(201),
	[string.char(150)] = string.char(202),
	[string.char(151)] = string.char(203),
	[string.char(152)] = string.char(196),
	[string.char(153)] = string.char(197),
	[string.char(154)] = string.char(198),
	[string.char(155)] = string.char(199),
	[string.char(156)] = string.char(192),
	[string.char(157)] = string.char(193),
	[string.char(158)] = string.char(194),
	[string.char(159)] = string.char(195),
	[string.char(160)] = string.char(252),
	[string.char(161)] = string.char(253),
	[string.char(162)] = string.char(254),
	[string.char(163)] = string.char(255),
	[string.char(164)] = string.char(248),
	[string.char(165)] = string.char(249),
	[string.char(166)] = string.char(250),
	[string.char(167)] = string.char(251),
	[string.char(168)] = string.char(244),
	[string.char(169)] = string.char(245),
	[string.char(170)] = string.char(246),
	[string.char(171)] = string.char(247),
	[string.char(172)] = string.char(240),
	[string.char(173)] = string.char(241),
	[string.char(174)] = string.char(242),
	[string.char(175)] = string.char(243),
	[string.char(176)] = string.char(236),
	[string.char(177)] = string.char(237),
	[string.char(178)] = string.char(238),
	[string.char(179)] = string.char(239),
	[string.char(180)] = string.char(232),
	[string.char(181)] = string.char(233),
	[string.char(182)] = string.char(234),
	[string.char(183)] = string.char(235),
	[string.char(184)] = string.char(228),
	[string.char(185)] = string.char(229),
	[string.char(186)] = string.char(230),
	[string.char(187)] = string.char(231),
	[string.char(188)] = string.char(224),
	[string.char(189)] = string.char(225),
	[string.char(190)] = string.char(226),
	[string.char(191)] = string.char(227),
	[string.char(192)] = string.char(156),
	[string.char(193)] = string.char(157),
	[string.char(194)] = string.char(158),
	[string.char(195)] = string.char(159),
	[string.char(196)] = string.char(152),
	[string.char(197)] = string.char(153),
	[string.char(198)] = string.char(154),
	[string.char(199)] = string.char(155),
	[string.char(200)] = string.char(148),
	[string.char(201)] = string.char(149),
	[string.char(202)] = string.char(150),
	[string.char(203)] = string.char(151),
	[string.char(204)] = string.char(144),
	[string.char(205)] = string.char(145),
	[string.char(206)] = string.char(146),
	[string.char(207)] = string.char(147),
	[string.char(208)] = string.char(140),
	[string.char(209)] = string.char(141),
	[string.char(210)] = string.char(142),
	[string.char(211)] = string.char(143),
	[string.char(212)] = string.char(136),
	[string.char(213)] = string.char(137),
	[string.char(214)] = string.char(138),
	[string.char(215)] = string.char(139),
	[string.char(216)] = string.char(132),
	[string.char(217)] = string.char(133),
	[string.char(218)] = string.char(134),
	[string.char(219)] = string.char(135),
	[string.char(220)] = string.char(128),
	[string.char(221)] = string.char(129),
	[string.char(222)] = string.char(130),
	[string.char(223)] = string.char(131),
	[string.char(224)] = string.char(188),
	[string.char(225)] = string.char(189),
	[string.char(226)] = string.char(190),
	[string.char(227)] = string.char(191),
	[string.char(228)] = string.char(184),
	[string.char(229)] = string.char(185),
	[string.char(230)] = string.char(186),
	[string.char(231)] = string.char(187),
	[string.char(232)] = string.char(180),
	[string.char(233)] = string.char(181),
	[string.char(234)] = string.char(182),
	[string.char(235)] = string.char(183),
	[string.char(236)] = string.char(176),
	[string.char(237)] = string.char(177),
	[string.char(238)] = string.char(178),
	[string.char(239)] = string.char(179),
	[string.char(240)] = string.char(172),
	[string.char(241)] = string.char(173),
	[string.char(242)] = string.char(174),
	[string.char(243)] = string.char(175),
	[string.char(244)] = string.char(168),
	[string.char(245)] = string.char(169),
	[string.char(246)] = string.char(170),
	[string.char(247)] = string.char(171),
	[string.char(248)] = string.char(164),
	[string.char(249)] = string.char(165),
	[string.char(250)] = string.char(166),
	[string.char(251)] = string.char(167),
	[string.char(252)] = string.char(160),
	[string.char(253)] = string.char(161),
	[string.char(254)] = string.char(162),
	[string.char(255)] = string.char(163),
}

local xor_with_0x36 = {
	[string.char(0)] = string.char(54),
	[string.char(1)] = string.char(55),
	[string.char(2)] = string.char(52),
	[string.char(3)] = string.char(53),
	[string.char(4)] = string.char(50),
	[string.char(5)] = string.char(51),
	[string.char(6)] = string.char(48),
	[string.char(7)] = string.char(49),
	[string.char(8)] = string.char(62),
	[string.char(9)] = string.char(63),
	[string.char(10)] = string.char(60),
	[string.char(11)] = string.char(61),
	[string.char(12)] = string.char(58),
	[string.char(13)] = string.char(59),
	[string.char(14)] = string.char(56),
	[string.char(15)] = string.char(57),
	[string.char(16)] = string.char(38),
	[string.char(17)] = string.char(39),
	[string.char(18)] = string.char(36),
	[string.char(19)] = string.char(37),
	[string.char(20)] = string.char(34),
	[string.char(21)] = string.char(35),
	[string.char(22)] = string.char(32),
	[string.char(23)] = string.char(33),
	[string.char(24)] = string.char(46),
	[string.char(25)] = string.char(47),
	[string.char(26)] = string.char(44),
	[string.char(27)] = string.char(45),
	[string.char(28)] = string.char(42),
	[string.char(29)] = string.char(43),
	[string.char(30)] = string.char(40),
	[string.char(31)] = string.char(41),
	[string.char(32)] = string.char(22),
	[string.char(33)] = string.char(23),
	[string.char(34)] = string.char(20),
	[string.char(35)] = string.char(21),
	[string.char(36)] = string.char(18),
	[string.char(37)] = string.char(19),
	[string.char(38)] = string.char(16),
	[string.char(39)] = string.char(17),
	[string.char(40)] = string.char(30),
	[string.char(41)] = string.char(31),
	[string.char(42)] = string.char(28),
	[string.char(43)] = string.char(29),
	[string.char(44)] = string.char(26),
	[string.char(45)] = string.char(27),
	[string.char(46)] = string.char(24),
	[string.char(47)] = string.char(25),
	[string.char(48)] = string.char(6),
	[string.char(49)] = string.char(7),
	[string.char(50)] = string.char(4),
	[string.char(51)] = string.char(5),
	[string.char(52)] = string.char(2),
	[string.char(53)] = string.char(3),
	[string.char(54)] = string.char(0),
	[string.char(55)] = string.char(1),
	[string.char(56)] = string.char(14),
	[string.char(57)] = string.char(15),
	[string.char(58)] = string.char(12),
	[string.char(59)] = string.char(13),
	[string.char(60)] = string.char(10),
	[string.char(61)] = string.char(11),
	[string.char(62)] = string.char(8),
	[string.char(63)] = string.char(9),
	[string.char(64)] = string.char(118),
	[string.char(65)] = string.char(119),
	[string.char(66)] = string.char(116),
	[string.char(67)] = string.char(117),
	[string.char(68)] = string.char(114),
	[string.char(69)] = string.char(115),
	[string.char(70)] = string.char(112),
	[string.char(71)] = string.char(113),
	[string.char(72)] = string.char(126),
	[string.char(73)] = string.char(127),
	[string.char(74)] = string.char(124),
	[string.char(75)] = string.char(125),
	[string.char(76)] = string.char(122),
	[string.char(77)] = string.char(123),
	[string.char(78)] = string.char(120),
	[string.char(79)] = string.char(121),
	[string.char(80)] = string.char(102),
	[string.char(81)] = string.char(103),
	[string.char(82)] = string.char(100),
	[string.char(83)] = string.char(101),
	[string.char(84)] = string.char(98),
	[string.char(85)] = string.char(99),
	[string.char(86)] = string.char(96),
	[string.char(87)] = string.char(97),
	[string.char(88)] = string.char(110),
	[string.char(89)] = string.char(111),
	[string.char(90)] = string.char(108),
	[string.char(91)] = string.char(109),
	[string.char(92)] = string.char(106),
	[string.char(93)] = string.char(107),
	[string.char(94)] = string.char(104),
	[string.char(95)] = string.char(105),
	[string.char(96)] = string.char(86),
	[string.char(97)] = string.char(87),
	[string.char(98)] = string.char(84),
	[string.char(99)] = string.char(85),
	[string.char(100)] = string.char(82),
	[string.char(101)] = string.char(83),
	[string.char(102)] = string.char(80),
	[string.char(103)] = string.char(81),
	[string.char(104)] = string.char(94),
	[string.char(105)] = string.char(95),
	[string.char(106)] = string.char(92),
	[string.char(107)] = string.char(93),
	[string.char(108)] = string.char(90),
	[string.char(109)] = string.char(91),
	[string.char(110)] = string.char(88),
	[string.char(111)] = string.char(89),
	[string.char(112)] = string.char(70),
	[string.char(113)] = string.char(71),
	[string.char(114)] = string.char(68),
	[string.char(115)] = string.char(69),
	[string.char(116)] = string.char(66),
	[string.char(117)] = string.char(67),
	[string.char(118)] = string.char(64),
	[string.char(119)] = string.char(65),
	[string.char(120)] = string.char(78),
	[string.char(121)] = string.char(79),
	[string.char(122)] = string.char(76),
	[string.char(123)] = string.char(77),
	[string.char(124)] = string.char(74),
	[string.char(125)] = string.char(75),
	[string.char(126)] = string.char(72),
	[string.char(127)] = string.char(73),
	[string.char(128)] = string.char(182),
	[string.char(129)] = string.char(183),
	[string.char(130)] = string.char(180),
	[string.char(131)] = string.char(181),
	[string.char(132)] = string.char(178),
	[string.char(133)] = string.char(179),
	[string.char(134)] = string.char(176),
	[string.char(135)] = string.char(177),
	[string.char(136)] = string.char(190),
	[string.char(137)] = string.char(191),
	[string.char(138)] = string.char(188),
	[string.char(139)] = string.char(189),
	[string.char(140)] = string.char(186),
	[string.char(141)] = string.char(187),
	[string.char(142)] = string.char(184),
	[string.char(143)] = string.char(185),
	[string.char(144)] = string.char(166),
	[string.char(145)] = string.char(167),
	[string.char(146)] = string.char(164),
	[string.char(147)] = string.char(165),
	[string.char(148)] = string.char(162),
	[string.char(149)] = string.char(163),
	[string.char(150)] = string.char(160),
	[string.char(151)] = string.char(161),
	[string.char(152)] = string.char(174),
	[string.char(153)] = string.char(175),
	[string.char(154)] = string.char(172),
	[string.char(155)] = string.char(173),
	[string.char(156)] = string.char(170),
	[string.char(157)] = string.char(171),
	[string.char(158)] = string.char(168),
	[string.char(159)] = string.char(169),
	[string.char(160)] = string.char(150),
	[string.char(161)] = string.char(151),
	[string.char(162)] = string.char(148),
	[string.char(163)] = string.char(149),
	[string.char(164)] = string.char(146),
	[string.char(165)] = string.char(147),
	[string.char(166)] = string.char(144),
	[string.char(167)] = string.char(145),
	[string.char(168)] = string.char(158),
	[string.char(169)] = string.char(159),
	[string.char(170)] = string.char(156),
	[string.char(171)] = string.char(157),
	[string.char(172)] = string.char(154),
	[string.char(173)] = string.char(155),
	[string.char(174)] = string.char(152),
	[string.char(175)] = string.char(153),
	[string.char(176)] = string.char(134),
	[string.char(177)] = string.char(135),
	[string.char(178)] = string.char(132),
	[string.char(179)] = string.char(133),
	[string.char(180)] = string.char(130),
	[string.char(181)] = string.char(131),
	[string.char(182)] = string.char(128),
	[string.char(183)] = string.char(129),
	[string.char(184)] = string.char(142),
	[string.char(185)] = string.char(143),
	[string.char(186)] = string.char(140),
	[string.char(187)] = string.char(141),
	[string.char(188)] = string.char(138),
	[string.char(189)] = string.char(139),
	[string.char(190)] = string.char(136),
	[string.char(191)] = string.char(137),
	[string.char(192)] = string.char(246),
	[string.char(193)] = string.char(247),
	[string.char(194)] = string.char(244),
	[string.char(195)] = string.char(245),
	[string.char(196)] = string.char(242),
	[string.char(197)] = string.char(243),
	[string.char(198)] = string.char(240),
	[string.char(199)] = string.char(241),
	[string.char(200)] = string.char(254),
	[string.char(201)] = string.char(255),
	[string.char(202)] = string.char(252),
	[string.char(203)] = string.char(253),
	[string.char(204)] = string.char(250),
	[string.char(205)] = string.char(251),
	[string.char(206)] = string.char(248),
	[string.char(207)] = string.char(249),
	[string.char(208)] = string.char(230),
	[string.char(209)] = string.char(231),
	[string.char(210)] = string.char(228),
	[string.char(211)] = string.char(229),
	[string.char(212)] = string.char(226),
	[string.char(213)] = string.char(227),
	[string.char(214)] = string.char(224),
	[string.char(215)] = string.char(225),
	[string.char(216)] = string.char(238),
	[string.char(217)] = string.char(239),
	[string.char(218)] = string.char(236),
	[string.char(219)] = string.char(237),
	[string.char(220)] = string.char(234),
	[string.char(221)] = string.char(235),
	[string.char(222)] = string.char(232),
	[string.char(223)] = string.char(233),
	[string.char(224)] = string.char(214),
	[string.char(225)] = string.char(215),
	[string.char(226)] = string.char(212),
	[string.char(227)] = string.char(213),
	[string.char(228)] = string.char(210),
	[string.char(229)] = string.char(211),
	[string.char(230)] = string.char(208),
	[string.char(231)] = string.char(209),
	[string.char(232)] = string.char(222),
	[string.char(233)] = string.char(223),
	[string.char(234)] = string.char(220),
	[string.char(235)] = string.char(221),
	[string.char(236)] = string.char(218),
	[string.char(237)] = string.char(219),
	[string.char(238)] = string.char(216),
	[string.char(239)] = string.char(217),
	[string.char(240)] = string.char(198),
	[string.char(241)] = string.char(199),
	[string.char(242)] = string.char(196),
	[string.char(243)] = string.char(197),
	[string.char(244)] = string.char(194),
	[string.char(245)] = string.char(195),
	[string.char(246)] = string.char(192),
	[string.char(247)] = string.char(193),
	[string.char(248)] = string.char(206),
	[string.char(249)] = string.char(207),
	[string.char(250)] = string.char(204),
	[string.char(251)] = string.char(205),
	[string.char(252)] = string.char(202),
	[string.char(253)] = string.char(203),
	[string.char(254)] = string.char(200),
	[string.char(255)] = string.char(201),
}

-------------------------------------------------------------------------

function sha256.sha256(msg)
	msg = preproc(msg, #msg)
	local H = initH256( { })
	for i = 1, #msg, 64 do digestblock(msg, i, H) end
	return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) ..
	num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
end

function sha256.sha256_binary(msg)
	return hex_to_binary(sha256.sha256(msg))
end

function sha256.hmac_sha256(key, text)
	assert(type(key) == 'string', "key passed to hmac_sha256 should be a string")
	assert(type(text) == 'string', "text passed to hmac_sha256 should be a string")

	if #key > blocksize then
		key = sha256.sha256_binary(key)
	end

	local key_xord_with_0x36 = key:gsub('.', xor_with_0x36) .. string.rep(string.char(0x36), blocksize - #key)
	local key_xord_with_0x5c = key:gsub('.', xor_with_0x5c) .. string.rep(string.char(0x5c), blocksize - #key)

	return sha256.sha256(key_xord_with_0x5c .. sha256.sha256_binary(key_xord_with_0x36 .. text))
end


return sha256
