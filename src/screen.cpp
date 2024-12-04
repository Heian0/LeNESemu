// Fill out your copyright notice in the Description page of Project Settings.


#include "screen.h"

// Sets default values for this component's properties
Uscreen::Uscreen()
{
	cart = std::make_shared<Cartridge>();

	FString FilePath = FPaths::ProjectSavedDir() + TEXT("Roms/smb.nes");
	std::string fp = TCHAR_TO_UTF8(*FilePath);

	UE_LOG(LogTemp, Log, TEXT("filepath to rom: %s"), *FilePath);
	cart->initialize(fp);
	nes.insert_cartridge(cart);
	nes.reset();


	PrimaryComponentTick.bCanEverTick = true;

}


// Called when the game starts
void Uscreen::BeginPlay()
{
	UE_LOG(LogTemp, Log, TEXT("Hello there!"));

	Super::BeginPlay();

    CreateDynamicTexture();
	
}


// Called every frame
void Uscreen::TickComponent(float DeltaTime, ELevelTick TickType, FActorComponentTickFunction* ThisTickFunction)
{
	Super::TickComponent(DeltaTime, TickType, ThisTickFunction);

    HandleControllerInput();

    if (bEmulationRun)
    {
        do { nes.clock(); } while (!nes.ppu.frame_complete);
        nes.ppu.frame_complete = false;
    }

    TArray<FColor> PixelData = nes.ppu.get_screen();
    UpdateDynamicTexture(PixelData);
}


void Uscreen::HandleControllerInput()
{
    APlayerController* PlayerController = GetWorld()->GetFirstPlayerController();

    if (PlayerController)
    {
        nes.controller[0] = 0x00;

        if (PlayerController->IsInputKeyDown(EKeys::X)) { nes.controller[0] |= 0x80; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::Z)) { nes.controller[0] |= 0x40; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::A)) { nes.controller[0] |= 0x20; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::S)) { nes.controller[0] |= 0x10; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::I)) { nes.controller[0] |= 0x08; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::K)) { nes.controller[0] |= 0x03; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::J)) { nes.controller[0] |= 0x02; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::L)) { nes.controller[0] |= 0x01; }
        else { nes.controller[0] |= 0x00; }

        if (PlayerController->IsInputKeyDown(EKeys::SpaceBar))
        {
            UE_LOG(LogTemp, Log, TEXT("Spacebar is being pressed!"));

            bEmulationRun = true;
        }

    }
}


void Uscreen::CreateDynamicTexture()
{
    // Step 1: Create a 64x64 transient texture
    TvTexture = UTexture2D::CreateTransient(256, 240, PF_B8G8R8A8);
    if (!TvTexture)
    {
        UE_LOG(LogTemp, Error, TEXT("Failed to create dynamic texture!"));
        return;
    }
    TvTexture->CompressionSettings = TextureCompressionSettings::TC_VectorDisplacementmap;
    TvTexture->AddToRoot();
    TvTexture->UpdateResource();
    UE_LOG(LogTemp, Log, TEXT("Dynamic texture created successfully!"));

    AActor* Owner = GetOwner();
    // Step 2: Find the plane's StaticMeshComponent
    UStaticMeshComponent* MeshComponent = Owner->FindComponentByClass<UStaticMeshComponent>();
    if (!MeshComponent)
    {
        UE_LOG(LogTemp, Error, TEXT("Failed to find StaticMeshComponent on the actor!"));
        return;
    }
    UE_LOG(LogTemp, Log, TEXT("StaticMeshComponent found successfully!"));

    // Step 3: Check material slots
    if (MeshComponent->GetNumMaterials() <= 0)
    {
        UE_LOG(LogTemp, Error, TEXT("No materials available on the StaticMeshComponent!"));
        return;
    }

    // Step 4: Create and bind the dynamic material instance
    TvMaterialInstance = MeshComponent->CreateAndSetMaterialInstanceDynamic(0);
    if (!TvMaterialInstance)
    {
        UE_LOG(LogTemp, Error, TEXT("Failed to create material instance!"));
        return;
    }
    UE_LOG(LogTemp, Log, TEXT("Material instance created successfully!"));

    TvMaterialInstance->SetTextureParameterValue(FName("ScreenTexture"), TvTexture);
    UE_LOG(LogTemp, Log, TEXT("Dynamic texture successfully bound to material!"));

}

void Uscreen::UpdateDynamicTexture(const TArray<FColor>& PixelData)
{
    if (!TvTexture || PixelData.Num() != 256 * 240)
    {
        UE_LOG(LogTemp, Error, TEXT("Invalid inputs: Ensure the texture is valid and the color array matches the dimensions."));
        return;
    }

    // Ensure the texture is transient and has the correct format
    if (TvTexture->GetSizeX() != 256 || TvTexture->GetSizeY() != 240)
    {
        UE_LOG(LogTemp, Error, TEXT("Texture dimensions do not match the provided width and height."));
        return;
    }

    FUpdateTextureRegion2D Region(0, 0, 0, 0, 256, 240);

    // Lock the texture's bulk data for writing
    FTexture2DMipMap& Mip = TvTexture->GetPlatformData()->Mips[0];
    void* TextureData = Mip.BulkData.Lock(LOCK_READ_WRITE);

    // Copy the pixel data from the FColor array to the texture
    FMemory::Memcpy(TextureData, PixelData.GetData(), PixelData.Num() * sizeof(FColor));

    // Unlock the texture's bulk data
    Mip.BulkData.Unlock();
    // Update the texture resource to apply changes
    TvTexture->UpdateResource();
}

