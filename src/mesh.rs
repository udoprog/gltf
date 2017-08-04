
// Copyright 2017 The gltf Library Developers
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::borrow::Cow;
use std::collections::hash_map;
use std::{iter, slice};
use {accessor, extensions, json, material};

use accessor::{Accessor, DataType, Dimensions};
use Gltf;

pub use json::mesh::{Mode, Semantic};

trait Normalized {
    type Denormalized;
    fn denormalize(&self) -> Self::Denormalized;
}

impl Normalized for u8 {
    type Denormalized = f32;
    fn denormalize(&self) -> Self::Denormalized {
        *self as f32 / 255.0
    }
}

impl Normalized for u16 {
    type Denormalized = f32;
    fn denormalize(&self) -> Self::Denormalized {
        *self as f32 / 65535.0
    }
}

impl<T: Copy + Normalized> Normalized for [T; 2] {
    type Denormalized = [T::Denormalized; 2];
    fn denormalize(&self) -> Self::Denormalized {
        [
            self[0].denormalize(),
            self[1].denormalize(),
        ]
    }
}

impl<T: Copy + Normalized> Normalized for [T; 3] {
    type Denormalized = [T::Denormalized; 3];
    fn denormalize(&self) -> Self::Denormalized {
        [
            self[0].denormalize(),
            self[1].denormalize(),
            self[2].denormalize(),
        ]
    }
}

impl<T: Copy + Normalized> Normalized for [T; 4] {
    type Denormalized = [T::Denormalized; 4];
    fn denormalize(&self) -> Self::Denormalized {
        [
            self[0].denormalize(),
            self[1].denormalize(),
            self[2].denormalize(),
            self[3].denormalize(),
        ]
    }
}

/// A type that iterates over vertex attribute data.
pub type Iter<'a, T> = Ordered<'a, accessor::Iter<'a, T>>;

/// XYZ vertex normals of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct Normals<'a>(Ordered<'a, Maybe<'a, [f32; 3]>>);

/// XYZ vertex normal displacements of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct NormalDisplacements<'a>(Iter<'a, [f32; 3]>);

/// XYZ vertex positions of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct Positions<'a>(Iter<'a, [f32; 3]>);

/// XYZ vertex position displacements of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct PositionDisplacements<'a>(Iter<'a, [f32; 3]>);

/// XYZW vertex tangents of type `[f32; 4]` where the `w` component is a
/// sign value (-1 or +1) indicating the handedness of the tangent basis.
#[derive(Clone, Debug)]
pub struct Tangents<'a>(Maybe<'a, [f32; 4]>);

/// XYZ vertex tangent displacements of type `[f32; 3]`.
#[derive(Clone, Debug)]
pub struct TangentDisplacements<'a>(Iter<'a, [f32; 3]>);

/// Vertex colors.
#[derive(Clone, Debug)]
pub enum Colors<'a> {
    /// RGB vertex color of type `[u8; 3]>`.
    RgbU8(Iter<'a, [u8; 3]>),

    /// RGBA vertex color of type `[u8; 4]>`.
    RgbaU8(Iter<'a, [u8; 4]>),

    /// RGB vertex color of type `[u16; 3]>`.
    RgbU16(Iter<'a, [u16; 3]>),

    /// RGBA vertex color of type `[u16; 4]>`.
    RgbaU16(Iter<'a, [u16; 4]>),

    /// RGB vertex color of type `[f32; 3]`.
    RgbF32(Iter<'a, [f32; 3]>),

    /// RGBA vertex color of type `[f32; 4]`.
    RgbaF32(Iter<'a, [f32; 4]>),
}

/// Vertex joints.
/// Refer to the documentation on morph targets and skins for more
/// information.
#[derive(Clone, Debug)]
pub enum Joints<'a> {
    /// Joints of type `[u8; 4]`.
    /// Refer to the documentation on morph targets and skins for more
    /// information.
    U8(Iter<'a, [u8; 4]>),
    
    /// Joints of type `[u16; 4]`.
    /// Refer to the documentation on morph targets and skins for more
    /// information.
    U16(Iter<'a, [u16; 4]>),
}

/// UV texture co-ordinates.
#[derive(Clone, Debug)]
enum GenericTexCoords<'a> {
    /// UV texture co-ordinates of type `[f32; 2]`.
    F32(accessor::Iter<'a, [f32; 2]>),

    /// UV texture co-ordinates of type `[u8; 2]>`.
    U8(accessor::Iter<'a, [u8; 2]>),

    /// UV texture co-ordinates of type `[u16; 2]>`.
    U16(accessor::Iter<'a, [u16; 2]>),
}

/// UV texture co-ordinates.
#[derive(Clone, Debug)]
pub enum TexCoords<'a> {
    /// UV texture co-ordinates of type `[f32; 2]`.
    F32(Iter<'a, [f32; 2]>),

    /// UV texture co-ordinates of type `[u8; 2]>`.
    U8(Iter<'a, [u8; 2]>),

    /// UV texture co-ordinates of type `[u16; 2]>`.
    U16(Iter<'a, [u16; 2]>),
}

/// Index data.
#[derive(Clone, Debug)]
pub enum Indices<'a> {
    /// Index data of type U8
    U8(accessor::Iter<'a, u8>),
    /// Index data of type U16
    U16(accessor::Iter<'a, u16>),
    /// Index data of type U32
    U32(accessor::Iter<'a, u32>),
}

/// Defines the vertex ordering of a `Primitive`.
#[derive(Clone, Debug)]
enum Order<'a> {
    /// Begin from 0, increment by one.
    Regular,

    /// Custom vertex order.
    Custom(Cow<'a, [u32]>),
}

impl Order<'static> {
    /// Returns a borrowed version of this `Order`.
    pub fn borrow<'a>(&'a self) -> Order<'a> {
        use std::borrow::Borrow;
        match *self {
            Order::Regular => Order::Regular,
            Order::Custom(ref owned) => Order::Custom(Cow::from(owned.borrow())),
        }
    }
}

/// An `Iterator` that traverses another iterator with the given `Order`.
#[derive(Clone, Debug)]
pub enum Ordered<'a, I: Clone + Iterator> {
    /// Begin from 0, increment by one.
    Regular {
        /// The iterator we're reading from.
        iter: I,
    },

    /// Custom vertex order.
    Custom {
        /// The iterator we're reading from.
        iter: I,

        /// The iteration order.
        order: Cow<'a, [u32]>,

        /// The index of the next order index.
        index: usize,
    },
}

impl<'a, I: Clone + Iterator> Ordered<'a, I> {
    /// Constructs an `Ordered` iterator adaptor.
    fn new(order: Order<'a>, iter: I) -> Self {
        match order {
            Order::Regular => Ordered::Regular { iter },
            Order::Custom(order) => Ordered::Custom {
                iter,
                order,
                index: 0,
            },
        }
    }
}

impl<'a, I> Iterator for Ordered<'a, I>
    where I: Clone + Iterator
{
    type Item = I::Item;
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Ordered::Regular { ref mut iter } => iter.next(),
            Ordered::Custom { ref iter, ref order, ref mut index } => {
                let order_index = index.clone();
                *index += 1;
                order
                    .get(order_index)
                    .map(|item_index| {
                        iter.clone()
                            .nth(*item_index as usize)
                            .unwrap()
                    })
            },
        }
    }
}

/// Intermediate type for data that is either provided or generated.
#[derive(Clone, Debug)]
pub enum Maybe<'a, T: 'a> {
    /// The data was provided (and hence borrowed from an `Accessor`.)
    Provided(accessor::Iter<'a, T>),

    /// The data was generated (and hence owned by the `Primitive`.)
    Generated(slice::Iter<'a, T>),
}

impl<'a, T: Copy> Iterator for Maybe<'a, T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Maybe::Provided(ref mut iter) => iter.next(),
            Maybe::Generated(ref mut iter) => iter.next().map(|x| *x),
        }
    }
}

/// An `Iterator` that coerces index data to `u32`s.
#[derive(Clone, Debug)]
pub struct IndicesU32<'a>(Indices<'a>);

impl<'a> Iterator for IndicesU32<'a> {
    type Item = u32;
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            &mut Indices::U8(ref mut iter) => iter.next().map(|x| x as u32),
            &mut Indices::U16(ref mut iter) => iter.next().map(|x| x as u32),
            &mut Indices::U32(ref mut iter) => iter.next(),
        }
    }
}

/// An `Iterator` that coerces texture co-ordinates to `f32`s.
#[derive(Clone, Debug)]
pub struct TexCoordsF32<'a>(TexCoords<'a>);

impl<'a> Iterator for TexCoordsF32<'a> {
    type Item = [f32; 2];
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            &mut TexCoords::U8(ref mut i) => i.next().map(|x| x.denormalize()),
            &mut TexCoords::U16(ref mut i) => i.next().map(|x| x.denormalize()),
            &mut TexCoords::F32(ref mut i) => i.next(),
        }
    }
}

/// An `Iterator` that coerces texture co-ordinates to `f32`s.
#[derive(Clone, Debug)]
pub struct GenericTexCoordsF32<'a>(GenericTexCoords<'a>);

impl<'a> Iterator for GenericTexCoordsF32<'a> {
    type Item = [f32; 2];
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.0 {
            &mut GenericTexCoords::U8(ref mut i) => i.next().map(|x| x.denormalize()),
            &mut GenericTexCoords::U16(ref mut i) => i.next().map(|x| x.denormalize()),
            &mut GenericTexCoords::F32(ref mut i) => i.next(),
        }
    }
}

/// Weights,
/// Refer to the documentation on morph targets for more information.
#[derive(Clone, Debug)]
pub enum Weights<'a> {
    /// Weights of type `[f32; 4]`.
    F32(Iter<'a, [f32; 4]>),

    /// Weights of type `[u8; 4]`.
    U8(Iter<'a, [u8; 4]>),

    /// Weights of type `[u16; 4]`.
    U16(Iter<'a, [u16; 4]>),
}

/// Vertex attribute data.
#[derive(Clone, Debug)]
pub enum Attribute<'a> {
    /// Vertex colors.
    Colors(u32, Colors<'a>),

    // TODO: Handle extras (needs to be handled elsewhere to avoid taking lifetime)
    // #[cfg(feature = "extras")]
    // Extras(&'a str, accessor::Accessor),

    /// Vertex joints.
    /// Refer to the documentation on morph targets and skins for more
    /// information.
    Joints(u32, Joints<'a>),

    /// XYZ vertex positions of type `[f32; 3]`.
    Positions(Positions<'a>),

    /// XYZ vertex normals of type `[f32; 3]`.
    Normals(Normals<'a>),

    /// XYZW vertex tangents of type `[f32; 4]` where the `w` component is a
    /// sign value (-1 or +1) indicating the handedness of the tangent basis.
    Tangents(Tangents<'a>),

    /// UV texture co-ordinates.
    TexCoords(u32, TexCoords<'a>),

    /// Weights.
    /// Refer to the documentation on morph targets for more information.
    Weights(u32, Weights<'a>),
}

/// Morph targets.
#[derive(Clone, Debug)]
pub struct MorphTarget<'a> {
    /// XYZ vertex position displacements.
    positions: Option<PositionDisplacements<'a>>,

    /// XYZ vertex normal displacements.
    normals: Option<NormalDisplacements<'a>>,

    /// XYZ vertex tangent displacements.
    tangents: Option<TangentDisplacements<'a>>,
}

/// A set of primitives to be rendered.  A node can contain one or more meshes and
/// its transform places the meshes in the scene.
#[derive(Clone, Debug)]
pub struct Mesh<'a>  {
    /// The parent `Gltf` struct.
    gltf: &'a Gltf,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::mesh::Mesh,
}

/// An `Iterator` that visits the attributes of a `Primitive`.
#[derive(Clone, Debug)]
pub struct Attributes<'a> {
    /// The parent `Primitive` struct.
    prim: &'a Primitive<'a>,

    /// The internal attribute iter.
    iter: hash_map::Iter<'a, json::mesh::Semantic, json::Index<json::accessor::Accessor>>,
}

/// An `Iterator` that visits the primitives of a `Mesh`.
#[derive(Clone, Debug)]
pub struct Primitives<'a>  {
    /// The parent `Mesh` struct.
    mesh: &'a Mesh<'a>,

    /// The internal JSON primitive iterator.
    iter: iter::Enumerate<slice::Iter<'a, json::mesh::Primitive>>,
}

/// Geometry to be rendered with the given material.
#[derive(Clone, Debug)]
pub struct Primitive<'a> {
    /// The parent `Mesh` struct.
    mesh: &'a Mesh<'a>,

    /// The corresponding JSON index.
    index: usize,

    /// The corresponding JSON struct.
    json: &'a json::mesh::Primitive,

    /// The vertex ordering.
    vertex_order: Order<'static>,

    /// Generated normals.
    generated_normals: Option<Vec<[f32; 3]>>,

    /// Generated tangents.
    generated_tangents: Option<Vec<[f32; 4]>>,
}

impl<'a> Mesh<'a>  {
    /// Constructs a `Mesh`.
    pub fn new(gltf: &'a Gltf, index: usize, json: &'a json::mesh::Mesh) -> Self {
        Self {
            gltf: gltf,
            index: index,
            json: json,
        }
    }

    /// Returns the internal JSON index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) ->  &json::mesh::Mesh {
        self.json
    }

    /// Extension specific data.
    pub fn extensions(&self) -> extensions::mesh::Mesh<'a> {
        extensions::mesh::Mesh::new(
            self.gltf,
            &self.json.extensions,
        )
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// Optional user-defined name for this object.
    #[cfg(feature = "names")]
    pub fn name(&self) -> Option<&str> {
        self.json.name.as_ref().map(String::as_str)
    }

    /// Defines the geometry to be renderered with a material.
    pub fn primitives(&'a self) -> Primitives<'a> {
        Primitives {
            mesh: self,
            iter: self.json.primitives.iter().enumerate(),
        }
    }

    /// Defines the weights to be applied to the morph targets.
    pub fn weights(&self) -> Option<&[f32]> {
        self.json.weights.as_ref().map(Vec::as_slice)
    }
}

/// Returns the attribute with the given semantic value.
fn find_accessor_with_semantic<'a>(
    gltf: &'a Gltf,
    json: &'a json::mesh::Primitive,
    semantic: Semantic,
) -> Option<accessor::Accessor<'a>> {
    json.attributes
        .iter()
        .find(|&(ref key, _)| key.as_ref().unwrap() == &semantic)
        .map(|(_, index)| gltf.accessors().nth(index.value()).unwrap())
}

impl<'a> Primitive<'a> {
    /// Constructs a `Primitive`.
    ///
    /// The primitive is divided into four major properties:
    ///
    /// * Where `POSITION` is provided, the primitive will contain
    ///   `Some(Positioning)` with computed flat normals if `NORMAL`
    ///   was not provided also.
    ///
    /// * Where `TANGENT` is provided, the primitive will contain
    ///   `Some(Texturing)` along with any provided texture co-ordinates.
    ///
    /// * Where `TANGENT` is not provided but `TEXCOORD_0` and `POSITION` is,
    ///   the primitive will contain `Some(Texturing)` with computed tangents
    ///   along with any provided texture co-ordinates.
    ///
    /// * Where `COLOR_0` is provided, the primitive will contain `Some(Coloring)`
    ///
    /// * Where `JOINTS_0` and `WEIGHTS_0` are provided, the primitive will contain
    ///   `Some(Skinning)`.
    pub fn new(
        mesh: &'a Mesh<'a>,
        index: usize,
        json: &'a json::mesh::Primitive,
    ) -> Self {
        let positions = find_accessor_with_semantic(
            mesh.gltf,
            json,
            Semantic::Positions,
        ).map(|accessor| unsafe { accessor.iter::<[f32; 3]>() });
        let normals = find_accessor_with_semantic(
            mesh.gltf,
            json,
            Semantic::Normals,
        ).map(|accessor| unsafe { accessor.iter::<[f32; 3]>() });
        let tangents = find_accessor_with_semantic(
            mesh.gltf,
            json,
            Semantic::Tangents,
        ).map(|accessor| unsafe { accessor.iter::<[f32; 4]>() });
        let tex_coords_0 = find_accessor_with_semantic(
            mesh.gltf,
            json,
            Semantic::TexCoords(0),
        ).map(|accessor| {
            GenericTexCoordsF32(GenericTexCoords::from_accessor(accessor))
        });
        let indices = json.indices
            .as_ref()
            .map(|index| {
                let accessor = mesh.gltf.accessors().nth(index.value()).unwrap();
                IndicesU32(Indices::from_accessor(accessor))
            });

        let generated_normals = match (normals.as_ref(), positions.as_ref()) {
            (None, Some(positions)) => {
                let generated_normals = unimplemented!();
                Some(generated_normals)
            },
            _ => None,
        };

        let (vertex_order, generated_tangents) = match (
            tangents.as_ref(),
            positions.as_ref(),
            normals.as_ref(),
            tex_coords_0.as_ref(),
        ) {
            (None, Some(positions), Some(normals), Some(tex_coords)) => {
                let (order, generated_tangents) = unimplemented!();
                (order, Some(generated_tangents))
            },
            _ => (Order::Regular, None),
        };

        Primitive {
            mesh,
            json,
            index,
            vertex_order,
            generated_normals,
            generated_tangents,
        }
    }

    /// Returns the internal JSON item.
    pub fn as_json(&self) ->  &json::mesh::Primitive {
        self.json
    }
    
    /// Returns the vertex colors of the given set.
    pub fn colors<'b>(&'b self, set: u32) -> Option<Colors<'b>> {
        self.find_accessor_with_semantic(Semantic::Colors(set))
            .map(|accessor| {
                Colors::from_accessor(self.vertex_order.borrow(), accessor)
            })
    }

    /// Returns the vertex texture co-ordinates of the given set.
    pub fn tex_coords<'b>(&'b self, set: u32) -> Option<TexCoords<'b>> {
        self.find_accessor_with_semantic(Semantic::TexCoords(set))
            .map(|accessor| {
                TexCoords::from_accessor(self.vertex_order.borrow(), accessor)
            })
    }

    /// Returns the vertex texture co-ordinates of the given set, coerced into `f32`
    /// values.
    pub fn tex_coords_f32<'b>(&'b self, set: u32) -> Option<TexCoordsF32<'b>> {
        self.tex_coords(set).map(|iter| TexCoordsF32(iter))
    }

    /// Returns the joint indices of the given set.
    pub fn joints<'b>(&'b self, set: u32) -> Option<Joints<'b>> {
        self.find_accessor_with_semantic(Semantic::Joints(set))
            .map(|accessor| {
                Joints::from_accessor(self.vertex_order.borrow(), accessor)
            })
    }

    /// Returns the joint weights of the given set.
    pub fn weights<'b>(&'b self, set: u32) -> Option<Weights<'b>> {
        self.find_accessor_with_semantic(Semantic::Weights(set))
            .map(|accessor| {
                Weights::from_accessor(self.vertex_order.borrow(), accessor)
            })
    }

    /// Returns the primitive indices.
    pub fn indices<'b>(&'b self) -> Option<Indices<'b>> {
        self.json.indices.as_ref().map(|index| {
            let accessor = self.mesh.gltf.accessors().nth(index.value()).unwrap();
            Indices::from_accessor(accessor)
        })
    }

    /// Returns the vertex texture co-ordinates of the given set, coerced into `f32`
    /// values.
    pub fn indices_u32<'b>(&'b self) -> Option<IndicesU32<'b>> {
        self.indices().map(|iter| IndicesU32(iter))
    }

    /// Returns the primitive positions.
    pub fn positions<'b>(&'b self) -> Option<Positions<'b>> {
        self.find_accessor_with_semantic(Semantic::Positions)
            .map(|accessor| unsafe {
                Positions(
                    Ordered::new(
                        self.vertex_order.borrow(),
                        accessor.iter(),
                    )
                )
            })
    }

    /// Returns the primitive normals.
    pub fn normals<'b>(&'b self) -> Option<Normals<'b>> {
        let vertex_order = self.vertex_order.borrow();
        if let Some(slice) = self.generated_normals.as_ref() {
            Some(
                Normals(
                    Ordered::new(
                        vertex_order,
                        Maybe::Generated(slice.iter()),
                    )
                )
            )
        } else if let Some(accessor) = self.find_accessor_with_semantic(Semantic::Normals) {
            Some(
                Normals(
                    Ordered::new(
                        vertex_order,
                        Maybe::Provided(unsafe { accessor.iter() }),
                    )
                )
            )
        } else {
            None
        }
    }

    /// Returns the primitive tangents.
    pub fn tangents<'b>(&'b self) -> Option<Tangents<'b>> {
        let vertex_order = self.vertex_order.borrow();
        if let Some(slice) = self.generated_tangents.as_ref() {
            Some(Tangents(Maybe::Generated(slice.iter())))
        } else if let Some(accessor) = self.find_accessor_with_semantic(Semantic::Tangents) {
            Some(Tangents(Maybe::Provided(unsafe { accessor.iter() })))
        } else {
            None
        }
    }

    /// Returns the attribute with the given semantic value.
    fn find_accessor_with_semantic(
        &self,
        semantic: Semantic,
    ) -> Option<accessor::Accessor<'a>> {
        self.json.attributes
            .iter()
            .find(|&(ref key, _)| key.as_ref().unwrap() == &semantic)
            .map(|(_, index)| self.mesh.gltf.accessors().nth(index.value()).unwrap())
    }

    /// Extension specific data.
    pub fn extensions(&self) -> extensions::mesh::Primitive<'a> {
        extensions::mesh::Primitive::new(
            self.mesh,
            &self.json.extensions,
        )
    }

    /// Optional application specific data.
    pub fn extras(&self) -> &json::Extras {
        &self.json.extras
    }

    /// The material to apply to this primitive when rendering
    pub fn material(&self) -> material::Material<'a> {
        self.json.material
            .as_ref()
            .map(|index| self.mesh.gltf.materials().nth(index.value()).unwrap())
            .unwrap_or_else(|| material::Material::default(self.mesh.gltf))
    }

    /// The type of primitives to render.
    pub fn mode(&self) -> Mode {
        self.json.mode.unwrap()
    }
}

impl<'a> Colors<'a> {
    fn from_accessor(order: Order<'a>, accessor: Accessor<'a>) -> Colors<'a> {
        unsafe {
            match (accessor.dimensions(), accessor.data_type()) {
                (Dimensions::Vec3, DataType::U8) => {
                    Colors::RgbU8(Ordered::new(order, accessor.iter()))
                },
                (Dimensions::Vec4, DataType::U8) => {
                    Colors::RgbaU8(Ordered::new(order, accessor.iter()))
                },
                (Dimensions::Vec3, DataType::U16) => {
                    Colors::RgbU16(Ordered::new(order, accessor.iter()))
                },
                (Dimensions::Vec4, DataType::U16) => {
                    Colors::RgbaU16(Ordered::new(order, accessor.iter()))
                },
                (Dimensions::Vec3, DataType::F32) => {
                    Colors::RgbF32(Ordered::new(order, accessor.iter()))
                },
                (Dimensions::Vec4, DataType::F32) => {
                    Colors::RgbaF32(Ordered::new(order, accessor.iter()))
                },
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> TexCoords<'a> {
    fn from_accessor(order: Order<'a>, accessor: Accessor<'a>) -> TexCoords<'a> {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => TexCoords::U8(Ordered::new(order, accessor.iter())),
                DataType::U16 => TexCoords::U16(Ordered::new(order, accessor.iter())),
                DataType::F32 => TexCoords::F32(Ordered::new(order, accessor.iter())),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> GenericTexCoords<'a> {
    fn from_accessor(accessor: Accessor<'a>) -> GenericTexCoords<'a> {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => GenericTexCoords::U8(accessor.iter()),
                DataType::U16 => GenericTexCoords::U16(accessor.iter()),
                DataType::F32 => GenericTexCoords::F32(accessor.iter()),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> Indices<'a> {
    fn from_accessor(accessor: Accessor<'a>) -> Indices<'a> {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => Indices::U8(accessor.iter()),
                DataType::U16 => Indices::U16(accessor.iter()),
                DataType::U32 => Indices::U32(accessor.iter()),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> Joints<'a> {
    fn from_accessor(order: Order<'a>, accessor: Accessor<'a>) -> Joints<'a> {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => Joints::U8(Ordered::new(order, accessor.iter())),
                DataType::U16 => Joints::U16(Ordered::new(order, accessor.iter())),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> Weights<'a> {
    fn from_accessor(order: Order<'a>, accessor: Accessor<'a>) -> Weights<'a> {
        unsafe {
            match accessor.data_type() {
                DataType::U8 => Weights::U8(Ordered::new(order, accessor.iter())),
                DataType::U16 => Weights::U16(Ordered::new(order, accessor.iter())),
                DataType::F32 => Weights::F32(Ordered::new(order, accessor.iter())),
                _ => unreachable!(),
            }
        }
    }
}

impl<'a> Iterator for Positions<'a> {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for PositionDisplacements<'a> {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for Normals<'a> {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}
 
impl<'a> Iterator for NormalDisplacements<'a> {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for Tangents<'a> {
    type Item = [f32; 4];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for TangentDisplacements<'a> {
    type Item = [f32; 3];
    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

impl<'a> Iterator for Primitives<'a> {
    type Item = Primitive<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .map(|(index, json)| Primitive::new(self.mesh, index, json))
    }
}
