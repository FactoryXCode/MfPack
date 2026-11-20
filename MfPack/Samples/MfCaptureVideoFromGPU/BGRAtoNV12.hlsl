struct VSInput {
    float2 pos  : POSITION;
    float2 tex  : TEXCOORD0;
};

struct PSInput {
    float4 pos  : SV_POSITION;
    float2 tex  : TEXCOORD0;
};

Texture2D texSrc : register(t0);
SamplerState samLinear : register(s0);

PSInput VSMain(VSInput input)
{
    PSInput o;
    o.pos = float4(input.pos, 0.0, 1.0);
    o.tex = input.tex;
    return o;
}

struct PSOut {
    float Y    : SV_Target0;   // R8_UNORM
    float2 UV  : SV_Target1;   // R8G8_UNORM
};

PSOut PSMain(PSInput input)
{
    float4 c = texSrc.Sample(samLinear, input.tex); // BGRA or RGBA depending on duplication

    // Desktop duplication is usually B8G8R8A8_UNORM
    float B = c.b;
    float G = c.g;
    float R = c.r;

    // BT.601 (0-1 range)
    float Y  =  0.2990 * R + 0.5870 * G + 0.1140 * B;
    float Cb = -0.168736 * R - 0.331264 * G + 0.500000 * B + 0.5;
    float Cr =  0.500000 * R - 0.418688 * G - 0.081312 * B + 0.5;

    PSOut o;
    o.Y  = saturate(Y);                // Y plane 0..1
    o.UV = saturate(float2(Cb, Cr));   // UV plane 0..1
    return o;
}
