class CreateBackends < ActiveRecord::Migration
  def self.up
    create_table :backends do |t|

      t.timestamps
    end
  end

  def self.down
    drop_table :backends
  end
end
