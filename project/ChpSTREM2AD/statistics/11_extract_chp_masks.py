from pathlib import Path

import nibabel as nib
import numpy as np


FS_ROOT = Path(r"I:\researchR\project\ChpSTREM2AD\document\MRIImage\data\FreeSurfer")
OUT_ROOT = Path(r"I:\researchR\project\ChpSTREM2AD\document\MRIImage\data\mask")

# FreeSurfer aseg labels
LEFT_CHOROID_PLEXUS = 31
RIGHT_CHOROID_PLEXUS = 63


def main() -> None:
    OUT_ROOT.mkdir(parents=True, exist_ok=True)
    aseg_files = sorted(FS_ROOT.glob(r"*\mri\aseg.mgz"))
    if not aseg_files:
        raise SystemExit(f"No aseg.mgz files found under {FS_ROOT}")

    generated = []
    for aseg_path in aseg_files:
        subject_id = aseg_path.parent.parent.name
        img = nib.load(str(aseg_path))
        data = np.asanyarray(img.dataobj)
        mask = np.isin(data, [LEFT_CHOROID_PLEXUS, RIGHT_CHOROID_PLEXUS]).astype(np.uint8)
        out_path = OUT_ROOT / f"{subject_id}.nii.gz"
        out_img = nib.Nifti1Image(mask, img.affine)
        out_img.header.set_data_dtype(np.uint8)
        nib.save(out_img, str(out_path))
        generated.append(out_path)

    print(f"Generated {len(generated)} masks in {OUT_ROOT}")
    for path in generated:
        print(path.name)


if __name__ == "__main__":
    main()
