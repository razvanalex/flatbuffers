<?php
// automatically generated by the FlatBuffers compiler, do not modify

use \Google\FlatBuffers\Struct;
use \Google\FlatBuffers\Table;
use \Google\FlatBuffers\ByteBuffer;
use \Google\FlatBuffers\FlatBufferBuilder;
use \Google\FlatBuffers\Constants;
use \Google\FlatBuffers\IUnpackableObject;
use \Google\FlatBuffers\IGeneratedObject;

class HandFanT implements IGeneratedObject
{
    /**
     * @var int $length
     */
    public $length;

    /**
     * @param int $length
     */
    public function __construct($length = 0)
    {
        $this->length = $length;
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return int offset
     */
    public function pack(FlatBufferBuilder $builder)
    {
        HandFan::startHandFan($builder);
        HandFan::addLength($builder, $this->length);
        $handFan = HandFan::endHandFan($builder);
        return $handFan;
    }
}

class HandFan extends Table implements IUnpackableObject
{
    /**
     * @param ByteBuffer $bb
     * @return HandFan
     */
    public static function getRootAsHandFan(ByteBuffer $bb)
    {
        $obj = new HandFan();
        return $obj->init($bb->getInt($bb->getPosition()) + $bb->getPosition(), $bb);
    }

    /**
     * @param ByteBuffer $bb
     * @return HandFan
     */
    public static function getSizePrefixedRootAsHandFan(ByteBuffer $bb)
    {
        $obj = new HandFan();
        $bb->setPosition($bb->getPosition() + Constants::SIZEOF_INT);
        return $obj->init($bb->getInt($bb->getPosition()) + $bb->getPosition(), $bb);
    }

    public static function HandFanIdentifier()
    {
        return "MOVI";
    }

    public static function HandFanBufferHasIdentifier(ByteBuffer $buf)
    {
        return self::__has_identifier($buf, self::HandFanIdentifier());
    }

    /**
     * @param int $_i offset
     * @param ByteBuffer $_bb
     * @return HandFan
     **/
    public function init($_i, ByteBuffer $_bb)
    {
        $this->bb_pos = $_i;
        $this->bb = $_bb;
        return $this;
    }

    /**
     * @return int
     */
    public function getLength()
    {
        $o = $this->__offset(4);
        return $o != 0 ? $this->bb->getInt($o + $this->bb_pos) : 0;
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return void
     */
    public static function startHandFan(FlatBufferBuilder $builder)
    {
        $builder->startObject(1);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return HandFan
     */
    public static function createHandFan(FlatBufferBuilder $builder, $length)
    {
        $builder->startObject(1);
        self::addLength($builder, $length);
        $o = $builder->endObject();
        return $o;
    }

    /**
     * @param FlatBufferBuilder $builder
     * @param int
     * @return void
     */
    public static function addLength(FlatBufferBuilder $builder, $length)
    {
        $builder->addIntX(0, $length, 0);
    }

    /**
     * @param FlatBufferBuilder $builder
     * @return int table offset
     */
    public static function endHandFan(FlatBufferBuilder $builder)
    {
        $o = $builder->endObject();
        return $o;
    }

    /**
     * @param HandFanT $o
     */
    public function unPackTo(&$o)
    {
        $o->length = $this->getLength();
    }

    /**
     * @return HandFanT
     */
    public function unPack()
    {
        $o = new HandFanT();
        $this->unPackTo($o);
        return $o;
    }
}
