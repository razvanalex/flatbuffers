<?php
// automatically generated by the FlatBuffers compiler, do not modify

use \Google\FlatBuffers\Struct;
use \Google\FlatBuffers\Table;
use \Google\FlatBuffers\ByteBuffer;
use \Google\FlatBuffers\FlatBufferBuilder;
use \Google\FlatBuffers\Constants;

class Movie extends Table
{
    /**
     * @param ByteBuffer $bb
     * @return Movie
     */
    public static function getRootAsMovie(ByteBuffer $bb)
    {
        $obj = new Movie();
        return $obj->init($bb->getInt($bb->getPosition()) + $bb->getPosition(), $bb);
    }

    /**
     * @param ByteBuffer $bb
     * @return Movie
     */
    public static function getSizePrefixedRootAsMovie(ByteBuffer $bb)
    {
        $obj = new Movie();
        $bb->setPosition($bb->getPosition() + Constants::SIZEOF_INT);
        return $obj->init($bb->getInt($bb->getPosition()) + $bb->getPosition(), $bb);
    }

    public static function MovieIdentifier()
    {
        return "MOVI";
    }

    public static function MovieBufferHasIdentifier(ByteBuffer $buf)
    {
        return self::__has_identifier($buf, self::MovieIdentifier());
    }

    /**
     * @param int $_i offset
     * @param ByteBuffer $_bb
     * @return Movie
     **/
    public function init($_i, ByteBuffer $_bb)
    {
        $this->bb_pos = $_i;
        $this->bb = $_bb;
        return $this;
    }

    /**
     * @return byte
     */
    public function getMainCharacterType()
    {
        $o = $this->__offset(4);
        return $o != 0 ? $this->bb->getByte($o + $this->bb_pos) : \Character::NONE;
    }

    /**
     * @returnint
     */
    public function getMainCharacter($obj)
    {
        $o = $this->__offset(6);
        return $o != 0 ? $this->__union($obj, $o) : null;
    }

    /**
     * @param int offset
     * @return byte
     */
    public function getCharactersType($j)
    {
        $o = $this->__offset(8);
        return $o != 0 ? $this->bb->getByte($this->__vector($o) + $j * 1) : \Character::NONE;
    }

    /**
     * @return int
     */
    public function getCharactersTypeLength()
    {
        $o = $this->__offset(8);
        return $o != 0 ? $this->__vector_len($o) : 0;
    }

    /**
     * @param int offset
     * @return Table
     */
    public function getCharacters($j, $obj)
    {
        $o = $this->__offset(10);
        return $o != 0 ? $this->__union($obj, $this->__vector($o) + $j * 4 - $this->bb_pos) : null;
    }

    /**
     * @return int
     */
    public function getCharactersLength()
    {
        $o = $this->__offset(10);
        return $o != 0 ? $this->__vector_len($o) : 0;
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return void
     */
    public static function startMovie(FlatBufferBuilder $builder)
    {
        $builder->StartObject(4);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return Movie
     */
    public static function createMovie(FlatBufferBuilder $builder, $main_character_type, $main_character, $characters_type, $characters)
    {
        $builder->startObject(4);
        self::addMainCharacterType($builder, $main_character_type);
        self::addMainCharacter($builder, $main_character);
        self::addCharactersType($builder, $characters_type);
        self::addCharacters($builder, $characters);
        $o = $builder->endObject();
        return $o;
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param byte
     * @return void
     */
    public static function addMainCharacterType(FlatBufferBuilder $builder, $mainCharacterType)
    {
        $builder->addByteX(0, $mainCharacterType, 0);
    }

    public static function addMainCharacter(FlatBufferBuilder $builder, $offset)
    {
        $builder->addOffsetX(1, $offset, 0);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param VectorOffset
     * @return void
     */
    public static function addCharactersType(FlatBufferBuilder $builder, $charactersType)
    {
        $builder->addOffsetX(2, $charactersType, 0);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param array offset array
     * @return int vector offset
     */
    public static function createCharactersTypeVector(FlatBufferBuilder $builder, array $data)
    {
        $builder->startVector(1, count($data), 1);
        for ($i = count($data) - 1; $i >= 0; $i--) {
            $builder->putByte($data[$i]);
        }
        return $builder->endVector();
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param int $numElems
     * @return void
     */
    public static function startCharactersTypeVector(FlatBufferBuilder $builder, $numElems)
    {
        $builder->startVector(1, $numElems, 1);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param VectorOffset
     * @return void
     */
    public static function addCharacters(FlatBufferBuilder $builder, $characters)
    {
        $builder->addOffsetX(3, $characters, 0);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param array offset array
     * @return int vector offset
     */
    public static function createCharactersVector(FlatBufferBuilder $builder, array $data)
    {
        $builder->startVector(4, count($data), 4);
        for ($i = count($data) - 1; $i >= 0; $i--) {
            $builder->putOffset($data[$i]);
        }
        return $builder->endVector();
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param int $numElems
     * @return void
     */
    public static function startCharactersVector(FlatBufferBuilder $builder, $numElems)
    {
        $builder->startVector(4, $numElems, 4);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return int table offset
     */
    public static function endMovie(FlatBufferBuilder $builder)
    {
        $o = $builder->endObject();
        return $o;
    }

    public static function finishMovieBuffer(FlatBufferBuilder $builder, $offset)
    {
        $builder->finish($offset, "MOVI");
    }

    public static function finishSizePrefixedMovieBuffer(FlatBufferBuilder $builder, $offset)
    {
        $builder->finish($offset, "MOVI", true);
    }
}
